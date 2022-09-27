# ubuntu 18.04... for now
FROM kernsuite/base:6

# need to test whether we can migrate to GCC 10
ENV GNUCOMPILER 6
RUN docker-apt-install build-essential\ 
                       g++-$GNUCOMPILER \
                       gcc-$GNUCOMPILER \
                       gfortran-$GNUCOMPILER \
		       python3-dev \
                       python3-all \
		       wcslib-dev \
                       python3-astropy \
                       python3-casacore \
                       casacore-data \
                       curl \
                       wget \
                       rsync \
                       python3-pycurl \
                       python3-matplotlib \
                       python3-numpy \
                       python3-ephem \
                       f2c \                                                               
                       libf2c2-dev \                                                       
                       bison \                                                             
                       flex \
                       python3-urllib3 

RUN update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-$GNUCOMPILER 100 && \
    update-alternatives --install /usr/bin/cc cc /usr/bin/gcc-$GNUCOMPILER 100 && \
    update-alternatives --install /usr/bin/x86_64-linux-gnu-gcc x86_64-linux-gnu-gcc /usr/bin/gcc-$GNUCOMPILER 100 && \
    update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-$GNUCOMPILER 100 && \
    update-alternatives --install /usr/bin/x86_64-linux-gnu-g++ x86_64-linux-gnu-g++ /usr/bin/g++-$GNUCOMPILER 100 && \
    update-alternatives --install /usr/bin/cpp cpp /usr/bin/g++-$GNUCOMPILER 100 && \
    update-alternatives --install /usr/bin/x86_64-linux-gnu-cpp x86_64-linux-gnu-cpp /usr/bin/g++-$GNUCOMPILER 100 && \
    update-alternatives --install /usr/bin/gfortran gfortran /usr/bin/gfortran-$GNUCOMPILER 100 && \
    update-alternatives --install /usr/bin/x86_64-linux-gnu-gfortran x86_64-linux-gnu-gfortran /usr/bin/gfortran-$GNUCOMPILER 100 && \
    update-alternatives --install /usr/bin/python python /usr/bin/python3.6 100

RUN mkdir -p /optsoft/bin && \
    mkdir -p /optsoft/lib && \
    mkdir -p /optsoft/src && \
    mkdir -p /optsoft/include

ENV PATH $PATH:/optsoft/bin
ENV LD_LIBRARY_PATH $LD_LIBRARY_PATH:/optsoft/lib
ENV C_INCLUDE_PATH $C_INCLUDE_PATH:/optsoft/include
ENV CPLUS_INCLUDE_PATH $C_INCLUDE_PATH:/optsoft/include

# ---------- Build RNXCMP from source into /optsoft ---------- 
RUN mkdir /src
ADD RNXCMP_4.0.5_src.tar.gz /src/
WORKDIR /src/RNXCMP_4.0.5_src/source
RUN gcc-$GNUCOMPILER -ansi -O2 -static rnx2crx.c -o /optsoft/bin/RNX2CRX && \
    gcc-$GNUCOMPILER -ansi -O2 -static crx2rnx.c -o /optsoft/bin/CRX2RNX

# ---------- COMPILE ALBUS ----------
# Step 1 setup directory structure and move source code into container
RUN mkdir /src/ALBUS
ENV ALBUSPATH /src/ALBUS

ADD IMF24_request.png $ALBUSPATH
ADD build_rnx_crx $ALBUSPATH
ADD dates $ALBUSPATH
ADD definitions $ALBUSPATH
ADD fast_orbital_data $ALBUSPATH
ADD INSTALL $ALBUSPATH
ADD Makefile $ALBUSPATH
ADD iri.update $ALBUSPATH
ADD kill_python $ALBUSPATH
ADD LICENSE $ALBUSPATH
ADD README.md $ALBUSPATH
ADD remove_remainder $ALBUSPATH
ADD test_iri.f $ALBUSPATH
ADD UPDATING_SPACE_WEATHER $ALBUSPATH
ADD bin $ALBUSPATH/bin
ADD C++ $ALBUSPATH/C++
ADD examples $ALBUSPATH/examples
ADD FORTRAN $ALBUSPATH/FORTRAN
ADD include $ALBUSPATH/include
ADD IRI_2012 $ALBUSPATH/IRI_2012
ADD libdata $ALBUSPATH/libdata
ADD Python $ALBUSPATH/Python
ADD python_scripts $ALBUSPATH/python_scripts
ADD share $ALBUSPATH/share

# Step 2 configure install path and apply necessary patches to build system
RUN mkdir /optsoft/ALBUS
ENV ALBUSINSTALL /optsoft/ALBUS

## Configure Make custom paths .. should really convert this to cmake or something....
RUN sed -i '15s/.*/export INSTALLDIR = '$(echo ${ALBUSINSTALL} | sed 's/\//\\\//g')'/' $ALBUSPATH/Makefile
#### Ubuntu 18.04 ships Python 3.6 LTS not 3.8 as it is defined in the build system
RUN sed -i '19s/.*/export CURRENT_PYTHON = python3.6/' $ALBUSPATH/Makefile
RUN sed -i '22s/.*/export PYTHONINCLUDEDIR = \/usr\/include\/python3.6/' $ALBUSPATH/Makefile
#### Build with specified GNU toolchain
RUN sed -i '39s/.*/export CC = gcc-'"${GNUCOMPILER}"'/' $ALBUSPATH/Makefile
RUN sed -i '40s/.*/export F77 = gfortran-'"${GNUCOMPILER}"' --std=legacy/' $ALBUSPATH/Makefile
RUN sed -i '43s/.*/export F77_RECL_UNIT = bytes/' $ALBUSPATH/Makefile
RUN sed -i '45s/.*/export C++ = g++-'"${GNUCOMPILER}"'/' $ALBUSPATH/Makefile
RUN sed -i '47s/.*/export CPP = cpp-'"${GNUCOMPILER}"' -P/' $ALBUSPATH/Makefile
## Patch up CFLAG passing... various missing includes and variable passing....
RUN sed -i '10s/.*/CFLAGS += -I$(PYTHONINCLUDEDIR) -I$(INSTALLDIR)\/include -DINSTALLDIR=\\"$(INSTALLDIR)\\"/' $ALBUSPATH/C++/mim/test/PIMrunner/Makefile
RUN sed -i '20s/.*/CFLAGS += -I$(PYTHONINCLUDEDIR) -I$(INSTALLDIR)\/include/' $ALBUSPATH/C++/AlbusIonosphere/python_attempt/Makefile
## Dead symlinks????
##> $ ls -lah share/python                                                                                                   [±cleanup_dockerize ●●]
##total 8.0K
##drwxrwxr-x 2 hugo hugo 4.0K Sep 26 11:44 .
##drwxrwxr-x 3 hugo hugo 4.0K Sep 26 11:44 ..
##lrwxrwxrwx 1 hugo hugo   42 Sep 26 11:44 Albus_Coordinates.py -> ../../maaijke_scripts/Albus_Coordinates.py
##lrwxrwxrwx 1 hugo hugo   38 Sep 26 11:44 Albus_RINEX_2.py -> ../../maaijke_scripts/Albus_RINEX_2.py
##lrwxrwxrwx 1 hugo hugo   36 Sep 26 11:44 Albus_RINEX.py -> ../../maaijke_scripts/Albus_RINEX.py
##lrwxrwxrwx 1 hugo hugo   37 Sep 26 11:44 GPS_stations.py -> ../../maaijke_scripts/GPS_stations.py
##lrwxrwxrwx 1 hugo hugo   34 Sep 26 11:44 jma_tools.py -> ../../maaijke_scripts/jma_tools.py
##lrwxrwxrwx 1 hugo hugo   36 Sep 26 11:44 MS_Iono_agw.py -> ../../maaijke_scripts/MS_Iono_agw.py

RUN rm $ALBUSPATH/share/python/*

# Step 3 Configure environment
ENV PATH "$ALBUSINSTALL/bin:$PATH"
ENV LD_LIBRARY_PATH "$ALBUSINSTALL/lib:$LD_LIBRARY_PATH"
ENV PYTHONPATH "$ALBUSINSTALL/share/python:$ALBUSINSTALL/lib:$PYTHONPATH"

# Step 4 Fingers crossed -- build
WORKDIR $ALBUSPATH
RUN make install
# TODO other stuffs prob -- unit tests -- integration tests???
RUN python -c "import AlbusIonosphere" && echo "Crack the bubbly - this hog is airborne!!!"
# crack the bubbly: this hog is airborne!



