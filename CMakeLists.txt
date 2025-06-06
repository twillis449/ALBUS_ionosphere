cmake_minimum_required(VERSION 3.10)
project(JMA_Code VERSION 1.0.0 LANGUAGES C CXX Fortran)

#----------------------------------------------------------------------------
# Configuration variables (equivalent to exported Makefile variables)
#----------------------------------------------------------------------------
# Installation directory (adjust as needed)
set(INSTALLDIR "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere" CACHE PATH "Installation directory")   ## set by docker file

find_package(PythonInterp REQUIRED)
find_package(PythonLibs REQUIRED)


# Python_VERSION_STRING is defined by FindPythonInterp
# It will be something like "3.6.9" → extract the major.minor part

# Extract just "python3.6" from the version
string(REGEX MATCH "^[0-9]+\\.[0-9]+" PYTHON_MAJOR_MINOR "${PYTHON_VERSION_STRING}")
set(CURRENT_PYTHON "python${PYTHON_MAJOR_MINOR}")



set(PYTHONINCLUDEDIR ${PYTHON_INCLUDE_DIR}) 


include_directories(${PYTHON_INCLUDE_DIRS})

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
    # Add legacy standard flag
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} --std=legacy")

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


# Build all libaries then link with ALBUS_ionosphere last in C++ folder 




add_subdirectory(FORTRAN)
add_subdirectory(Python)
add_subdirectory(C++)

##set file permissions after installation 
install(CODE "execute_process(COMMAND chmod -R ugo+rX ${INSTALLDIR}/bin)")
install(CODE "execute_process(COMMAND chmod -R ugo+rX ${INSTALLDIR}/include)")
install(CODE "execute_process(COMMAND chmod -R ugo+rX ${INSTALLDIR}/lib)")
install(CODE "execute_process(COMMAND chmod -R ugo+rX ${INSTALLDIR}/libdata)")
install(CODE "execute_process(COMMAND chmod -R ugo+rX ${INSTALLDIR}/share)")
install(CODE "execute_process(COMMAND mkdir -p ${INSTALLDIR}/man)")
install(CODE "execute_process(COMMAND chmod -R ugo+rX ${INSTALLDIR}/man)")


