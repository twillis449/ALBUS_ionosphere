cmake_minimum_required(VERSION 3.10)
project(AlbusIonosphere CXX)

#---------------------------------------------------------------------------
# Source File(s)
#---------------------------------------------------------------------------
set(SRCS AlbusIonosphere.cxx)


add_custom_target(python_install
  COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_SOURCE_DIR}
          /bin/sh -c
            "echo '${INSTALLDIR} ${VER_MAJOR} ${VER_MINOR}' | \
             '${CURRENT_PYTHON}' setup.py build install \
               --prefix='${INSTALLDIR}' \
               --install-platlib='${INSTALLDIR}/lib' \
               --install-purelib='${INSTALLDIR}/share/python'"
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  VERBATIM
)

install(CODE "execute_process(COMMAND ${CMAKE_COMMAND} --build . --target python_install)")




