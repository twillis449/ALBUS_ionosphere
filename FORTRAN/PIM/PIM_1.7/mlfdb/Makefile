#
# JMA_code.rules
#
# Example Makefile for JMA coding
# 2004 Feb 13  James M Anderson  --NRAO  reorganize from various places
# 2006 Jul 05  JMA  --JIVE  more changes for better installation stuff
# 2008 Feb 15  JMA  --MPIfR updates for generalizing build
# 2008 Feb 18  JMA  --MPIfR updates for generalizing build







CONVERTER = mlftounf
ifeq ($(F77_RECL_UNIT),bytes)
  CONVERTER_INPUT_EXT = in
else
  CONVERTER_INPUT_EXT = inw
endif






all: convert
.PHONY: all convert

convert:
	$(MAKE) -C utils $(CONVERTER)

install: utils/$(CONVERTER)
	$(CP) utils/$(CONVERTER) form
	cd form; ./$(CONVERTER) < ../utils/$(CONVERTER).$(CONVERTER_INPUT_EXT)
	$(MKDIR) $(INSTALLDIR)/libdata/PIM/mlfdb/unform
	$(MV) form/nm*.* form/sm*.* $(INSTALLDIR)/libdata/PIM/mlfdb/unform
	touch install










# This will probably work best with C and C++ code.  Modify
# If you want to do this for FORTRAN too.
.PHONY : depend clean distclean
depend:
	$(RM) $(MAKEDEPENDREAL)
	$(foreach i,$(SRCS), $(MAKEDEPEND) $(CFLAGS) $i;cat $(MAKEDEPENDTEMP) >> $(MAKEDEPENDREAL);)
	$(RM) $(MAKEDEPENDTEMP)


clean:
	$(RM) $(OBJS) $(PROG)
	$(RM) form/$(CONVERTER)
	$(RM) utils/$(CONVERTER)
	$(RM) utils/*.o
	$(RM) form/nm*.* form/sm*.* 
	$(RM) install

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
