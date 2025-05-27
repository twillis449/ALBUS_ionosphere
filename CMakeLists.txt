cmake_minimum_required(VERSION 3.10)
project(JMA_Code VERSION 1.0.0 LANGUAGES C CXX Fortran)

#----------------------------------------------------------------------------
# Configuration variables (equivalent to exported Makefile variables)
#----------------------------------------------------------------------------
# Installation directory (adjust as needed)
set(INSTALLDIR "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere" CACHE PATH "Installation directory")   ## set by docker file

# Python configuration
set(CURRENT_PYTHON "python" CACHE STRING "Python interpreter")   ## set by docker file
set(PYTHONINCLUDEDIR "/usr/include/python3.10" CACHE PATH "Python include directory")  ## set by docker file

# Version numbers for the library (if used in targets)
set(VER_MAJOR 1)
set(VER_MINOR 0)
set(VER_SUBSUB 0)

# Options for debugging and shared/static library build
option(DEBUG "Enable debugging" OFF)
option(SHARED "Build shared library" ON)

# Option for HAVE_G2C_H_FILE (defaulting to off)
set(HAVE_G2C_H_FILE 0 CACHE BOOL "Set if g2c.h is available")




include_directories(${CMAKE_SOURCE_DIR} ${INSTALLDIR}/include)

add_definitions(-DINSTALLDIR="${INSTALLDIR}")
if(HAVE_G2C_H_FILE)
  add_definitions(-DHAVE_G2C_H_FILE=${HAVE_G2C_H_FILE})
endif()

#----------------------------------------------------------------------------
# Set compiler flags based on system type and debug option
#----------------------------------------------------------------------------
if(CMAKE_SYSTEM_NAME MATCHES "Linux")
  if(NOT DEBUG)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O2 -Wall")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2 -Wall")
  else()
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -Wall")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -Wall")
  endif()
elseif(CMAKE_SYSTEM_NAME MATCHES "SunOS")
  # Solaris settings (adjust as needed)
  if(NOT DEBUG)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pipe -O2 -msupersparc -Wall")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O")
  else()
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -Wall")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g")
  endif()
endif()

#----------------------------------------------------------------------------
# For shared libraries, ensure position independent code
#----------------------------------------------------------------------------
if(SHARED)
  set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif()

#----------------------------------------------------------------------------
# Define a postfix for file names (mimicking the Makefile POSTFIX logic)
#----------------------------------------------------------------------------
if(NOT DEBUG)
  set(POSTFIX ${CMAKE_SYSTEM_NAME})
else()
  set(POSTFIX "${CMAKE_SYSTEM_NAME}_debug")
endif()

#----------------------------------------------------------------------------
# Subdirectories (similar to your SUBDIRS list in the Makefile)
#----------------------------------------------------------------------------
# Each of these subdirectories should contain its own CMakeLists.txt.

# Build all libaries then link with ALBUS_ionosphere last in C++ folder 

add_subdirectory(FORTRAN)
add_subdirectory(Python)
add_subdirectory(C++)



