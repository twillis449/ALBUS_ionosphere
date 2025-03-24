cmake_minimum_required(VERSION 3.10)
project(JMA_Code VERSION 1.0.0 LANGUAGES C CXX Fortran)

#----------------------------------------------------------------------------
# Configuration variables (equivalent to exported Makefile variables)
#----------------------------------------------------------------------------
# Installation directory (adjust as needed)
set(INSTALLDIR "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere" CACHE PATH "Installation directory")

# Python configuration
set(CURRENT_PYTHON "python" CACHE STRING "Python interpreter")
set(PYTHONINCLUDEDIR "/usr/include/python3.10" CACHE PATH "Python include directory")

# Version numbers for the library (if used in targets)
set(VER_MAJOR 1)
set(VER_MINOR 0)
set(VER_SUBSUB 0)

# Options for debugging and shared/static library build
option(DEBUG "Enable debugging" OFF)
option(SHARED "Build shared library" ON)

# Option for HAVE_G2C_H_FILE (defaulting to off)
set(HAVE_G2C_H_FILE 0 CACHE BOOL "Set if g2c.h is available")

#----------------------------------------------------------------------------
# Set compilers explicitly (if desired; these are normally auto-detected)
#----------------------------------------------------------------------------
# set(CMAKE_C_COMPILER gcc)
# set(CMAKE_CXX_COMPILER g++)
# For Fortran: note the Makefile uses some extra flags (e.g. -std=legacy)
# set(CMAKE_Fortran_COMPILER gfortran)

#----------------------------------------------------------------------------
# Set include directories and preprocessor definitions
#----------------------------------------------------------------------------
include_directories(${CMAKE_SOURCE_DIR} ${INSTALLDIR}/include)
add_definitions(-DINSTALLDIR="\"${INSTALLDIR}\"")
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
# Add PIC flag if building shared libraries; set file extension accordingly
#----------------------------------------------------------------------------
if(SHARED)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC")
  set(EXT "so")
else()
  set(EXT "a")
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
add_subdirectory("C++")
add_subdirectory("FORTRAN")
add_subdirectory("Python")
add_subdirectory("include")

#----------------------------------------------------------------------------
# Installation rules (install targets)
#----------------------------------------------------------------------------
# Here we assume that each subdirectory properly installs its targets.
# The following install commands mimic the manual chmod/mkdir in the Makefile.
install(DIRECTORY ${INSTALLDIR}/bin
        DESTINATION bin
        FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                         GROUP_READ GROUP_EXECUTE
                         WORLD_READ WORLD_EXECUTE
        DIRECTORY_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                              GROUP_READ GROUP_EXECUTE
                              WORLD_READ WORLD_EXECUTE)
install(DIRECTORY ${INSTALLDIR}/include
        DESTINATION include
        FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                         GROUP_READ GROUP_EXECUTE
                         WORLD_READ WORLD_EXECUTE
        DIRECTORY_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                              GROUP_READ GROUP_EXECUTE
                              WORLD_READ WORLD_EXECUTE)
install(DIRECTORY ${INSTALLDIR}/lib
        DESTINATION lib
        FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                         GROUP_READ GROUP_EXECUTE
                         WORLD_READ WORLD_EXECUTE
        DIRECTORY_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                              GROUP_READ GROUP_EXECUTE
                              WORLD_READ WORLD_EXECUTE)
install(DIRECTORY ${INSTALLDIR}/libdata
        DESTINATION libdata
        FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                         GROUP_READ GROUP_EXECUTE
                         WORLD_READ WORLD_EXECUTE
        DIRECTORY_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                              GROUP_READ GROUP_EXECUTE
                              WORLD_READ WORLD_EXECUTE)
# Create and install man and share directories
install(DIRECTORY ${INSTALLDIR}/man
        DESTINATION man
        FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                         GROUP_READ GROUP_EXECUTE
                         WORLD_READ WORLD_EXECUTE
        DIRECTORY_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                              GROUP_READ GROUP_EXECUTE
                              WORLD_READ WORLD_EXECUTE)
install(DIRECTORY ${INSTALLDIR}/share
        DESTINATION share
        FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                         GROUP_READ GROUP_EXECUTE
                         WORLD_READ WORLD_EXECUTE
        DIRECTORY_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                              GROUP_READ GROUP_EXECUTE
                              WORLD_READ WORLD_EXECUTE)

#----------------------------------------------------------------------------
# Custom targets for cleaning up (optional)
#----------------------------------------------------------------------------
# CMake already provides a "clean" target via the generated build system.
# The following custom target emulates "distclean" from the Makefile.
add_custom_target(distclean
  COMMAND ${CMAKE_COMMAND} --build . --target clean
  COMMAND ${CMAKE_COMMAND} -E remove_directory ${INSTALLDIR}
  COMMENT "Performing a distclean (cleaning build files and installation directory)"
)

