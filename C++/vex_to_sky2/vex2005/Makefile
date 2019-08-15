#
YACC=bison
YFLAGS=-d -y
LEX=flex
# HV: 16/11/2004 - removed some .o files whose object code we don't need in 
#                  JCCS
#OBJECTS=vex.o vexf.o vexf_put.o vex.yy.o vex_util.o print_vex.o vex_get.o vex_put.o
OBJECTS=vex.o vex.yy.o vex_util.o print_vex.o vex_get.o

HEADERS = vex++.h  vex.h  y.tab.h

# HV: 16/11/2004 - Over we'd like to use gcc 3.2
#CC=gcc
#CC=/local/gcc-3.2/bin/gcc
#
# F2C needs this others don't
# HV: 16/11/2004 - For linkage to JCCS, we require position independant
#                  code (-fPIC)
#
# Makefile
#
# Example Makefile for JMA coding
# 2004 Feb 13  James M Anderson  --NRAO  reorganize from various places
# 2005 Sep 20  JMA  --JIVE  set up for vex library
# 2008 Feb 15  JMA  --MPIfR updates for generalizing build




#CFLAGS=-DF2C
CFLAGS += -fPIC
#




LIBS = \
	libvex.$(EXT)



all: $(LIBS)
.PHONY : all





# HV: 16/11/2004 - Introduced two new targets:
#                   libvex.a and libvex.sl
libvex.a: $(OBJECTS)
	touch $@
	$(RM) $@
	ar -rcs $@ $(OBJECTS)

libvex.sl: $(OBJECTS)
	touch $@
	rm $@
	gcc  -fPIC -shared -o $@ $(OBJECTS)

libvex.so: $(OBJECTS)
	$(RM) $@.$(VER_MAJOR).$(VER_MINOR).$(VER_SUBSUB) $@.$(VER_MAJOR) $@ 
	gcc -shared -Wl,-soname,$@.$(VER_MAJOR) -o $@.$(VER_MAJOR).$(VER_MINOR).$(VER_SUBSUB) $(OBJECTS)
	ln -s $@.$(VER_MAJOR).$(VER_MINOR).$(VER_SUBSUB) $@.$(VER_MAJOR)
	ln -s $@.$(VER_MAJOR) $@ 

install: libvex.so
	$(CP) libvex.* $(INSTALLDIR)/lib
	$(CP) $(HEADERS) $(INSTALLDIR)/include



# this is original -
vex.a: $(OBJECTS)
	touch $@
	rm $@
	ar -qc $@ $(OBJECTS)
#
# GNU ar (Linux) needs this, others don't
	ar s $@
#
	rm -f vex.c vex.yy.c

vex.yy.o:	vex.yy.l y.tab.h

y.tab.h:	vex.y

vex_util.o:	y.tab.h

print_vex.o:	y.tab.h

vex_get.o:	y.tab.h

vex_put.o:	y.tab.h

vexf.o:		y.tab.h



.PHONY : depend clean distclean install

clean:
	$(RM) $(OBJECTS) vex.c vex.yy.c libvex.*

distclean: clean
	$(RM) *~

