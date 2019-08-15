#
# JMA_code.rules
#
# Example Makefile for JMA coding
# 2004 Feb 13  James M Anderson  --NRAO  reorganize from various places


# Version numbering for the libsofa library
VER_MAJOR = 1
VER_MINOR = 0
VER_SUBSUB = 0



SHELL = /bin/csh




# do not use debugging by default
DEBUG=0
# do not make a shared library by default
SHARED = 0
EXT = a



# define the compiler
CC = gcc
F77 = g77
C++ = g++

CPP = cpp -P

MAKEDEPENDREAL = .dependencies.mk
MAKEDEPENDTEMP = .dependencies.mk.temp
MAKEDEPEND = gcc -MM -MF $(MAKEDEPENDTEMP)




INCLUDES = -I. 
DEFINES = 









ifeq ($(OSTYPE),linux)
  ifeq ($(DEBUG),0)
    CFLAGS = -march=i686 -Os -Wall $(INCLUDES) $(DEFINES) 
    FFLAGS = -march=i686 -Os -Wall $(INCLUDES) $(DEFINES) -fno-second-underscore 
  else
    CFLAGS = -march=i686 -Os -g -Wall $(INCLUDES) $(DEFINES) 
    FFLAGS = -march=i686 -Os -g -Wall $(INCLUDES) $(DEFINES) -fno-second-underscore 
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





SRCS = \
	a2af.f \
	a2tf.f \
	anp.f \
	anpm.f \
	bi00.f \
	bp00.f \
	bpn2xy.f \
	c2i00a.f \
	c2i00b.f \
	c2ibpn.f \
	c2ixy.f \
	c2ixys.f \
	c2s.f \
	c2t00a.f \
	c2t00b.f \
	c2tceo.f \
	c2teqx.f \
	c2tpe.f \
	c2txy.f \
	cal2jd.f \
	cp.f \
	cpv.f \
	cr.f \
	d2tf.f \
	dat.f \
	dtdb.f \
	ee00.f \
	ee00a.f \
	ee00b.f \
	eect00.f \
	epb.f \
	epb2jd.f \
	epj.f \
	epj2jd.f \
	epv00.f \
	eqeq94.f \
	era00.f \
	fk52h.f \
	fk5hip.f \
	fk5hz.f \
	gmst00.f \
	gmst82.f \
	gst00a.f \
	gst00b.f \
	gst94.f \
	h2fk5.f \
	hfk5z.f \
	ir.f \
	jd2cal.f \
	jdcalf.f \
	num00a.f \
	num00b.f \
	numat.f \
	nut00a.f \
	nut00b.f \
	nut80.f \
	nutm80.f \
	obl80.f \
	p2pv.f \
	p2s.f \
	pap.f \
	pas.f \
	pdp.f \
	plan94.f \
	pm.f \
	pmat00.f \
	pmat76.f \
	pmp.f \
	pn.f \
	pn00.f \
	pn00a.f \
	pn00b.f \
	pnm00a.f \
	pnm00b.f \
	pnm80.f \
	pom00.f \
	ppp.f \
	ppsp.f \
	pr00.f \
	prec76.f \
	pv2p.f \
	pv2s.f \
	pvdpv.f \
	pvm.f \
	pvmpv.f \
	pvppv.f \
	pvstar.f \
	pvu.f \
	pvup.f \
	pvxpv.f \
	pxp.f \
	rm2v.f \
	rv2m.f \
	rx.f \
	rxp.f \
	rxpv.f \
	rxr.f \
	ry.f \
	rz.f \
	s00.f \
	s00a.f \
	s00b.f \
	s2c.f \
	s2p.f \
	s2pv.f \
	s2xpv.f \
	sepp.f \
	seps.f \
	sp00.f \
	starpm.f \
	starpv.f \
	sxp.f \
	sxpv.f \
	tr.f \
	trxp.f \
	trxpv.f \
	xys00a.f \
	xys00b.f \
	zp.f \
	zpv.f \
	zr.f


OBJS = \
	a2af.o \
	a2tf.o \
	anp.o \
	anpm.o \
	bi00.o \
	bp00.o \
	bpn2xy.o \
	c2i00a.o \
	c2i00b.o \
	c2ibpn.o \
	c2ixy.o \
	c2ixys.o \
	c2s.o \
	c2t00a.o \
	c2t00b.o \
	c2tceo.o \
	c2teqx.o \
	c2tpe.o \
	c2txy.o \
	cal2jd.o \
	cp.o \
	cpv.o \
	cr.o \
	d2tf.o \
	dat.o \
	dtdb.o \
	ee00.o \
	ee00a.o \
	ee00b.o \
	eect00.o \
	epb.o \
	epb2jd.o \
	epj.o \
	epj2jd.o \
	epv00.o \
	eqeq94.o \
	era00.o \
	fk52h.o \
	fk5hip.o \
	fk5hz.o \
	gmst00.o \
	gmst82.o \
	gst00a.o \
	gst00b.o \
	gst94.o \
	h2fk5.o \
	hfk5z.o \
	ir.o \
	jd2cal.o \
	jdcalf.o \
	num00a.o \
	num00b.o \
	numat.o \
	nut00a.o \
	nut00b.o \
	nut80.o \
	nutm80.o \
	obl80.o \
	p2pv.o \
	p2s.o \
	pap.o \
	pas.o \
	pdp.o \
	plan94.o \
	pm.o \
	pmat00.o \
	pmat76.o \
	pmp.o \
	pn.o \
	pn00.o \
	pn00a.o \
	pn00b.o \
	pnm00a.o \
	pnm00b.o \
	pnm80.o \
	pom00.o \
	ppp.o \
	ppsp.o \
	pr00.o \
	prec76.o \
	pv2p.o \
	pv2s.o \
	pvdpv.o \
	pvm.o \
	pvmpv.o \
	pvppv.o \
	pvstar.o \
	pvu.o \
	pvup.o \
	pvxpv.o \
	pxp.o \
	rm2v.o \
	rv2m.o \
	rx.o \
	rxp.o \
	rxpv.o \
	rxr.o \
	ry.o \
	rz.o \
	s00.o \
	s00a.o \
	s00b.o \
	s2c.o \
	s2p.o \
	s2pv.o \
	s2xpv.o \
	sepp.o \
	seps.o \
	sp00.o \
	starpm.o \
	starpv.o \
	sxp.o \
	sxpv.o \
	tr.o \
	trxp.o \
	trxpv.o \
	xys00a.o \
	xys00b.o \
	zp.o \
	zpv.o \
	zr.o

PROG = \

LIBS = \
	libsofa.$(EXT)



all: $(LIBS)


libsofa.so: $(OBJS)
	$(RM) $@.$(VER_MAJOR).$(VER_MINOR).$(VER_SUBSUB) $@.$(VER_MAJOR) $@ 
	gcc -shared -Wl,-soname,$@.$(VER_MAJOR) -o $@.$(VER_MAJOR).$(VER_MINOR).$(VER_SUBSUB) $(OBJS)
	ln -s $@.$(VER_MAJOR).$(VER_MINOR).$(VER_SUBSUB) $@.$(VER_MAJOR)
	ln -s $@.$(VER_MAJOR) $@ 


libsofa.a: $(OBJS)
	$(RM) $@
	ar rcs $@ $(OBJS)



#find_line_max: $(OBJS) find_line_max.o
#	$(CC) $(CFLAGS) -o $@ $(OBJS) find_line_max.o -lm -lsupc++






# This will probably work best with C and C++ code.  Modify
# If you want to do this for FORTRAN too.
depend:
	$(RM) $(MAKEDEPENDREAL)
	$(foreach i,$(SRCS), $(MAKEDEPEND) $(CFLAGS) $i;cat $(MAKEDEPENDTEMP) >> $(MAKEDEPENDREAL);)
	$(RM) $(MAKEDEPENDTEMP)


clean:
	$(RM) $(OBJS) $(PROG) $(LIBS)

distclean: 
	$(RM) $(OBJS) $(PROG) $(LIBS)
	$(RM) *~ libsofa.*




# make sure we can compile C++ stuff
.SUFFIXES:
.SUFFIXES: .cxx $(SUFFIXES)









# declare our own default compilation rules
.cxx.o:
	$(CC) $(CFLAGS) -c $<

.F.o:
	$(F77) $(FFLAGS) -c $<












# If you are going to use MAKEDEPEND with GCC, uncomment the following
#include .dependencies.mk



# The following stuff is for automatic generation of dependencies
# Some MAKEDEPEND programs need it.  I tend to use gcc to make
# it's own dependencies file.
# DO NOT DELETE THIS LINE -- make depend depends on it.
