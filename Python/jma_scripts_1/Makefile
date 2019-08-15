#
# JMA_code.rules
#
# Example Makefile for JMA coding
# 2004 Feb 13  James M Anderson  --NRAO  reorganize from various places
# 2006 Jul 05  JMA  --JIVE  more changes for better installation stuff
# 2008 Feb 15  JMA  --MPIfR updates for generalizing build



SRCS = \
	Albus_Coordinates.py \
	Albus_RINEX.py \
	Albus_RINEX_2.py \
	Albus_Wide_0.py \
	GPS_stations.py \
	jma_EVN.py \
	jma_aips.py 

MODULES =  \
	Albus_Coordinates \
	Albus_RINEX \
	Albus_RINEX_2 \
	Albus_Wide_0 \
	GPS_stations \
	jma_EVN \
	jma_aips 


PROG = \
	Albus_RINEX_download.py \
	Albus_RINEX_userftp.py



DATA = \
	gps_pos_default.snx







all: 
.PHONY: all


install: 
	$(MKDIR) $(INSTALLDIR)/share/python
	$(CP) $(SRCS) $(INSTALLDIR)/share/python
	$(MKDIR) $(INSTALLDIR)/libdata/JMA
	$(CP) $(DATA) $(INSTALLDIR)/libdata/JMA
	$(MKDIR) $(INSTALLDIR)/bin
	$(CP) $(PROG) $(INSTALLDIR)/bin
	$(foreach i,$(MODULES), cd $(INSTALLDIR)/share/python;echo import $i | $(PARSELTONGUE_PYTHON);)










# This will probably work best with C and C++ code.  Modify
# If you want to do this for FORTRAN too.
.PHONY : depend clean distclean install
depend:


clean:
	$(RM) $(OBJS)

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
