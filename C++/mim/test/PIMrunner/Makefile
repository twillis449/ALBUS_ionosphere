#
# JMA_code.rules
#
# Example Makefile for JMA coding
# 2004 Feb 13  James M Anderson  --NRAO  reorganize from various places
# 2008 Feb 15  JMA  --MPIfR updates for generalizing build

LIBRARY_NAME = mim
INCLUDES += -I$(PYTHONINCLUDEDIR) -I$(INSTALLDIR)/C++/vex_to_sky2
CFLAGS += -I$(PYTHONINCLUDEDIR)

#LIBRARIES = -L$(INSTALLDIR)/lib -lpim -liri -ljmavex -lvexplus -lvex -lsofa -lfl -l$(F77_COMPILER_LIB) -lm -larmadillo

LIBRARIES = -L$(INSTALLDIR)/lib -L/usr/lib -lpim -liri -ljmavex -lvexplus -lvex -lsofa -lfl -lm

HEADERS = \
	3C.h \
	GPS.h \
	GPS_collection.h \
	GPS_criteria.h \
	GPS_receiver.h \
	GPS_receiver_obs.h \
	GPS_satellites.h \
	GPS_times.h \
	MC_direction.h \
	MC_random.h \
	MT_random.h \
	VTEC.h \
	atmosphere_point.h \
	cal_source.h \
	data_calibrator.h \
	filter.h \
	iono_runner.h \
	ionosphere.h \
	ionosphere_fake.h \
	ionosphere_gen.h \
	ionosphere_gps.h \
	ionosphere_iri.h \
	ionosphere_iriplus.h \
	ionosphere_pim.h \
	latlon_cart.h \
	linfit.h \
	linfit_grad.h \
	location.h \
	observation.h \
	observation_3D.h \
	physical_constants.h \
	pim_runner.h \
	random_source.h \
	sofa_helpers.h \
	space_rotation_matrix.h \
	space_unit_vector.h \
	space_vector.h \
	station_latlon.h \
	station_maker.h \
	station_reference.h



SRCS = \
	3C.cxx \
	GPS.cxx \
	GPS_collection.cxx \
	GPS_receiver_obs.cxx \
	GPS_satellites.cxx \
	GPS_times.cxx \
	MC_direction.cxx \
	MT_random.cxx \
	VTEC.cxx \
	atmosphere_point.cxx \
	cal_source.cxx \
	data_calibrator.cxx \
	filter.cxx \
	iono_runner.cxx \
	ionosphere.cxx \
	ionosphere_fake.cxx \
	ionosphere_gen.cxx \
	ionosphere_gps.cxx \
	ionosphere_iri.cxx \
	ionosphere_iriplus.cxx \
	ionosphere_pim.cxx \
	jma_igrf13.c \
	latlon_cart.cxx \
	linfit.cxx \
	linfit_grad.cxx \
	physical_constants.cxx \
	pim_runner.cxx \
	random_source.cxx \
	sofa_helpers.cxx \
	space_rotation_matrix.cxx \
	space_unit_vector.cxx \
	space_vector.cxx \
	station_latlon.cxx \
	station_maker.cxx \
	station_reference.cxx \
	lofar.cxx \
	lofar2.cxx 

OBJS = \
	3C.o \
	GPS.o \
	GPS_collection.o \
	GPS_receiver_obs.o \
	GPS_satellites.o \
	GPS_times.o \
	MC_direction.o \
	MT_random.o \
	VTEC.o \
	atmosphere_point.o \
	cal_source.o \
	data_calibrator.o \
	filter.o \
	iono_runner.o \
	ionosphere.o \
	ionosphere_fake.o \
	ionosphere_gen.o \
	ionosphere_gps.o \
	ionosphere_iri.o \
	ionosphere_iriplus.o \
	ionosphere_pim.o \
	jma_igrf13.o \
	latlon_cart.o \
	linfit.o \
	linfit_grad.o \
	physical_constants.o \
	pim_runner.o \
	random_source.o \
	sofa_helpers.o \
	space_rotation_matrix.o \
	space_unit_vector.o \
	space_vector.o \
	station_latlon.o \
	station_maker.o \
	station_reference.o 

PROG = \
	lofar \
	lofar2 \
	lofar3 \
	lofar4 \
	lofar5 \
	lofar6 \
	lofar7 \
	lofar8 \
	lofar9 \
	lofar10 \
	test \
	test2 \
	vlba_obs


LIBS = \
	lib$(LIBRARY_NAME).$(EXT)

LOFAR_OBS = \
	lofar.o \
	lofar2.o \
	lofar3.o \
	lofar4.o \
	lofar5.o \
	lofar6.o \
	lofar7.o \
	lofar8.o \
	lofar9.o \
	lofar10.o \
#	armadillo \
	test.o \
	test2.o \
	vlba_obs.o

#all: $(PROG) $(LIBS)
all: $(LIBS)
.PHONY : all








lofar: $(OBJS) lofar.o
	$(C++) $(CFLAGS) -o $@ lofar.o $(OBJS) $(LIBRARIES)

lofar2: $(OBJS) lofar2.o
	$(C++) $(CFLAGS) -o $@ lofar2.o $(OBJS) $(LIBRARIES)

lofar3: $(OBJS) lofar3.o
	$(C++) $(CFLAGS) -o $@ lofar3.o $(OBJS) $(LIBRARIES)

lofar4: $(OBJS) lofar4.o
	$(C++) $(CFLAGS) -o $@ lofar4.o $(OBJS) $(LIBRARIES)

lofar5: $(OBJS) lofar5.o
	$(C++) $(CFLAGS) -o $@ lofar5.o $(OBJS) $(LIBRARIES)

lofar6: $(OBJS) lofar6.o
	$(C++) $(CFLAGS) -o $@ lofar6.o $(OBJS) $(LIBRARIES)

lofar7: $(OBJS) lofar7.o
	$(C++) $(CFLAGS) -o $@ lofar7.o $(OBJS) $(LIBRARIES)

lofar8: $(OBJS) lofar8.o
	$(C++) $(CFLAGS) -o $@ lofar8.cxx $(OBJS) $(LIBRARIES)

lofar9: $(OBJS) lofar9.o
	$(C++) $(CFLAGS) -o $@ lofar9.cxx $(OBJS) $(LIBRARIES)

lofar10: $(OBJS) lofar10.o
	$(C++) $(CFLAGS) -o $@ lofar10.cxx $(OBJS) $(LIBRARIES)

#armadillo: $(OBJS) armadillo.o
#	$(C++) $(CFLAGS) -o $@ armadillo.o $(OBJS) $(LIBRARIES)

vlba_obs: $(OBJS) vlba_obs.o
	$(C++) $(CFLAGS) -o $@ vlba_obs.cxx $(OBJS) $(LIBRARIES)


test: $(OBJS) test.o
	$(C++) $(CFLAGS) -o $@ test.o $(OBJS) $(LIBRARIES)

test2: $(OBJS) test2.o
	$(C++) $(CFLAGS) -o $@ test2.o $(OBJS) $(LIBRARIES)



lib$(LIBRARY_NAME).so: $(OBJS)
	$(RM) $@.$(VER_MAJOR).$(VER_MINOR).$(VER_SUBSUB) $@.$(VER_MAJOR) $@ 
	gcc -shared -Wl,-soname,$@.$(VER_MAJOR) -o $@.$(VER_MAJOR).$(VER_MINOR).$(VER_SUBSUB) $(OBJS) -L$(INSTALLDIR)/lib -ljmavex -liri -lpim -lvexplus -lvex -lfl -l$(F77_COMPILER_LIB)
	ln -s $@.$(VER_MAJOR).$(VER_MINOR).$(VER_SUBSUB) $@.$(VER_MAJOR)
	ln -s $@.$(VER_MAJOR) $@ 


lib$(LIBRARY_NAME).a: $(OBJS)
	$(RM) $@
	ar rcs $@ $(OBJS)

install: libmim.so
	$(CP) libmim.so* $(INSTALLDIR)/lib
	$(CP) $(HEADERS) $(INSTALLDIR)/include


#find_line_max: $(OBJS) find_line_max.o
#	$(CC) $(CFLAGS) -o $@ $(OBJS) find_line_max.o -lm -lsupc++






# This will probably work best with C and C++ code.  Modify
# If you want to do this for FORTRAN too.
.PHONY : depend clean distclean install
depend:
	$(RM) $(MAKEDEPENDREAL)
	$(foreach i,$(SRCS), $(MAKEDEPEND) $(CFLAGS) $i;cat $(MAKEDEPENDTEMP) >> $(MAKEDEPENDREAL);)
	$(RM) $(MAKEDEPENDTEMP)


clean:
	$(RM) $(OBJS) $(PROG) $(LOFAR_OBS) *.o  libmim.*

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
