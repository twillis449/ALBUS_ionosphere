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
                       ipython3 \
                       python3-ipdb \
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
                       python3-urllib3 \
                       unzip \
                       python3-nose \
                       python3-requests \
                       python3-pip \
                       cmake

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
ADD RNXCMP_4.1.0_src.tar.gz /src/
WORKDIR /src/RNXCMP_4.1.0_src/source
RUN gcc-$GNUCOMPILER -ansi -O2 -static rnx2crx.c -o /optsoft/bin/RNX2CRX && \
    gcc-$GNUCOMPILER -ansi -O2 -static crx2rnx.c -o /optsoft/bin/CRX2RNX && \
    ln -s /optsoft/bin/CRX2RNX /optsoft/bin/crx2rnx && \
    ln -s /optsoft/bin/RNX2CRX /optsoft/bin/rnx2crx


# ---------- COMPILE ALBUS ----------
# Step 1 setup directory structure and move source code into container
RUN mkdir /src/ALBUS
ENV ALBUSPATH /src/ALBUS

ADD IMF24_request.png $ALBUSPATH
#ADD cbuild $ALBUSPATH/cbuild
ADD build_rnx_crx $ALBUSPATH
ADD dates $ALBUSPATH
ADD definitions $ALBUSPATH
ADD fast_orbital_data $ALBUSPATH
ADD INSTALL $ALBUSPATH
#ADD Makefile $ALBUSPATH
ADD CMakeLists.txt $ALBUSPATH
ADD iri.update $ALBUSPATH
ADD kill_python $ALBUSPATH
ADD LICENSE $ALBUSPATH
ADD README.md $ALBUSPATH
#ADD remove_remainder $ALBUSPATH
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
# set up include dir
WORKDIR /src/ALBUS/include
RUN mkdir -p /optsoft/ALBUS/include
RUN cp  *.h /optsoft/ALBUS/include/



RUN mkdir -p /optsoft/ALBUS/bin
RUN mkdir -p /optsoft/ALBUS/lib
RUN mkdir -p /optsoft/ALBUS/libdata
RUN mkdir -p /optsoft/ALBUS/man
RUN mkdir -p /optsoft/ALBUS/share

## copy all data for libary
WORKDIR /src/ALBUS
RUN cp -r libdata /optsoft/ALBUS/libdata
ENV ALBUSINSTALL /optsoft/ALBUS

## Configure Make custom paths ..


RUN sed -i "8s|.*|set(INSTALLDIR \"${ALBUSINSTALL}\")|" $ALBUSPATH/CMakeLists.txt


#### Ubuntu 18.04 ships Python 3.6 LTS not 3.8 as it is defined in the build system

RUN sed -i '11s/.*/set(CURRENT_PYTHON python3.6)/' $ALBUSPATH/CMakeLists.txt
RUN sed -i '12s/.*/set(PYTHONINCLUDEDIR \/usr\/include\/python3.6)/' $ALBUSPATH/CMakeLists.txt
RUN sed -i '10s/.*/CFLAGS += -I$(PYTHONINCLUDEDIR) -I$(INSTALLDIR)\/include -DINSTALLDIR=\\"$(INSTALLDIR)\\"/' $ALBUSPATH/C++/mim/test/PIMrunner/Makefile

RUN rm $ALBUSPATH/share/python/*

# Step 3 Configure environment
ENV PATH "$ALBUSINSTALL/bin:$PATH"
ENV LD_LIBRARY_PATH "$ALBUSINSTALL/lib:$LD_LIBRARY_PATH"
ENV PYTHONPATH "$ALBUSINSTALL/share/python:$ALBUSINSTALL/lib:$PYTHONPATH"


# Step 4 Fingers crossed -- build
WORKDIR /src/ALBUS
RUN cmake .
RUN make install
RUN python -c "import AlbusIonosphere" && echo "Crack the bubbly - this hog is airborne!!!"

# can run any script mounted inside the container py calling (assuming you run Bash or equiv.)
# docker run -v <absolute path to gfzrnx>:/optsoft/bin/gfzrnx \
#            -v <absolute path to your waterhole with scripts>:/albus_waterhole \
#            --workdir /albus_waterhole \
#            --rm \
#            --user $(id -u <your user>):$(id -g <your user>) \
#            albus:latest <path to script mounted inside the waterhole>
# if you used 'albus as a tag when building the image'
# you must download and accept the license for gfzrnx separately 
# see https://dataservices.gfz-potsdam.de/panmetaworks/showshort.php?id=escidoc:1577894
# Note: adding -it in the flags above should give you an interactive python session
ENTRYPOINT [ "/usr/bin/python3.6" ]
# print some default stuffs
ENV HELPSTRING "docker run -v <absolute path to gfzrnx>:/optsoft/bin/gfzrnx "\
"-v <absolute path to your waterhole with scripts>:/albus_waterhole "\
"--workdir /albus_waterhole "\
"--rm "\
"--user $(id -u <your user>):$(id -g <your user>) "\
"albus:latest <path to script mounted inside the waterhole>"
CMD [ "-c", "import AlbusIonosphere; import os; import pkg_resources; print('ALBUS\\n====='); print(AlbusIonosphere.__doc__); version = pkg_resources.require('AlbusIonosphere')[0].version; print('Version {}'.format(version)); print('Usage: ' + os.environ['HELPSTRING'])" ]
# crack the bubbly: this hog is airborne!



