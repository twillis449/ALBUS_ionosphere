cmake_minimum_required(VERSION 3.10)
project(JMA_code_Python)

#-----------------------------------------------------------------------------
# Define lists of files and modules
#-----------------------------------------------------------------------------

set(SRCS
  Albus_rnx3_to_rnx2.py
  Albus_Coordinates.py
  MS_Iono_agw_azel_par.py
  Albus_RINEX.py
  Albus_RINEX_2.py
  GPS_stations.py
  Albus_iono_object.py
  MS_Iono_functions.py
  jma_tools.py
)

set(MODULES
  Albus_rnx3_to_rnx2
  Albus_Coordinates
  MS_Iono_agw_azel_par
  Albus_RINEX
  Albus_RINEX_2
  GPS_stations
  Albus_iono_object
  MS_Iono_functions
)

set(PROG Albus_RINEX_download.py)
set(DATA gps_pos_default.snx)



#-----------------------------------------------------------------------------
# Installation of files
#
# - Python scripts go to INSTALLDIR/share/python
# - Data files go to INSTALLDIR/libdata/JMA
# - The main program script goes to INSTALLDIR/bin
#-----------------------------------------------------------------------------

install(
  FILES ${SRCS}
  DESTINATION ${INSTALLDIR}/share/python
)



install(
  FILES ${PROG}
  DESTINATION ${INSTALLDIR}/bin
)
#need to this file executable
install(CODE "execute_process(COMMAND chmod +x ${INSTALLDIR}/bin/Albus_RINEX_download.py)")

# Precompile Python modules
#
# For each module listed in MODULES, the original Makefile does:
#   cd $(INSTALLDIR)/share/python; echo import <module> | $(CURRENT_PYTHON)
#
# Here we use a custom target that changes directory to the installed
# share/python directory and runs a shell loop to “import” each module
# with the Python interpreter.
#-----------------------------------------------------------------------------

#set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
#find_package(Python3 COMPONENTS Interpreter REQUIRED)


# Join MODULES into a space‐separated string for the shell loop.
string(REPLACE ";" " " MODULES_STR "${MODULES}")

# Optionally, you might want the precompile step to run as part of installation.
# Uncomment the following line to add a dependency so that 'install' depends on precompilation.
# add_dependencies(install install_precompile)

#-----------------------------------------------------------------------------
# Custom cleaning targets for Python artifacts
#
# These mimic the 'clean' and 'distclean' targets of the Makefile.
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# Note:
#
# - CMake’s built-in "clean" target (e.g. via "cmake --build . --target clean")
#   handles build artifacts, so here we only define custom targets for cleaning
#   Python-specific temporary files.
#
# - To install, configure your project (optionally setting INSTALLDIR) and run:
#       cmake --build . --target install
#   To run the precompile step manually:
#       cmake --build . --target install_precompile
#-----------------------------------------------------------------------------
