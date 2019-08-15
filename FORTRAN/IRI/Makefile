#
# JMA_code.rules
#
# Example Makefile for JMA coding
# 2005 Sep 13  James M Anderson  --JIVE  start for IRI
# 2008 Feb 15  JMA  --MPIfR updates for generalizing build

HEADERS = \
	jma_irisub_c.h


SRCS = \
	cira.f \
	igrf.f \
	iridreg.f \
	irifun.f \
	irisub.f \
	irisubne.f \
	iritec.f \
	iritecne.f \
	jma_irisub.F \
	jma_irisub_c.c \
	jma_irisubne.F



OBJS = \
	cira.o \
	igrf.o \
	iridreg.o \
	irifun.o \
#	jma_irisub.o \
#	jma_irisubne.o

NOBJS = \
	irisub.o \
	iritec.o \
	jma_irisub.o \
	jma_irisub_c.o 

NEOBJS = \
	irisubne.o \
	iritecne.o 


PROG = \
	iritest \
	iritestne \
	make_ap

LIBS = \
	libiri.$(EXT)




all: $(PROG) $(LIBS)
.PHONY : all


iritest: $(OBJS) $(NOBJS) iritest.o
	$(F77) $(FFLAGS) -o $@ iritest.o $(OBJS) $(NOBJS) $(LIBRARIES)

iritestne: $(OBJS) $(NEOBJS) iritestne.o
	$(F77) $(CFLAGS) -o $@ iritestne.o $(OBJS) $(NEOBJS) $(LIBRARIES)

libiri.so: $(OBJS) $(NOBJS) 
	$(RM) $@.$(VER_MAJOR).$(VER_MINOR).$(VER_SUBSUB) $@.$(VER_MAJOR) $@ 
	gcc -shared -Wl,-soname,$@.$(VER_MAJOR) -o $@.$(VER_MAJOR).$(VER_MINOR).$(VER_SUBSUB) $(OBJS) $(NOBJS) -l$(F77_COMPILER_LIB) -lm
	ln -s $@.$(VER_MAJOR).$(VER_MINOR).$(VER_SUBSUB) $@.$(VER_MAJOR)
	ln -s $@.$(VER_MAJOR) $@ 

jma_iri: $(OBJS) $(NOBJS) jma_irisub.o
	$(F77) $(FFLAGS) -o $@  $(OBJS) $(NOBJS) $(LIBRARIES)


libiri.a: $(OBJS) $(NOBJS) 
	$(RM) $@
	ar rcs $@ $(OBJS) $(NOBJS) 


install: libiri.so
	$(CP) libiri.* $(INSTALLDIR)/lib
	$(CP) $(HEADERS) $(INSTALLDIR)/include
	$(MKDIR) $(INSTALLDIR)/libdata/IRI
	$(CP) *.dat *.asc $(INSTALLDIR)/libdata/IRI





make_ap: make_ap.o
	$(C++) $(CFLAGS) -o $@ make_ap.o









# This will probably work best with C and C++ code.  Modify
# If you want to do this for FORTRAN too.
.PHONY : depend clean distclean install
depend:
	$(RM) $(MAKEDEPENDREAL)
	$(foreach i,$(SRCS), $(MAKEDEPEND) $(CFLAGS) $i;cat $(MAKEDEPENDTEMP) >> $(MAKEDEPENDREAL);)
	$(RM) $(MAKEDEPENDTEMP)


clean:
	$(RM) $(OBJS) $(PROG) $(NOBJS) $(NEOBJS) make_ap.o iritest.o iritestne.o libiri.so* 
#	$(RM) $(OBJS) $(PROG) $(NOBJS) $(NEOBJS) make_ap.o iritest.o iritestne.o 

distclean: clean
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
