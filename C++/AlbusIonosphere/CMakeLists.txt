cmake_minimum_required(VERSION 3.10)
project(Albus_Ionosphere CXX)

#---------------------------------------------------------------------------
# Version numbers (for informational purposes)
#---------------------------------------------------------------------------
set(VER_MAJOR 1)
set(VER_MINOR 0)
set(VER_SUBSUB 0)



#---------------------------------------------------------------------------
# Build Type and Compiler Flags
#
# Mimic the Makefile’s conditional flags for Linux and Solaris.
# If not specified, default to Release mode.
#---------------------------------------------------------------------------
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  if(CMAKE_BUILD_TYPE MATCHES "Release")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -march=i686 -O2 -Wall")
  else()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -march=i686 -O2 -g -Wall")
  endif()
elseif(CMAKE_SYSTEM_NAME STREQUAL "SunOS")
  if(CMAKE_BUILD_TYPE MATCHES "Release")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pipe -O2 -msupersparc -Wall")
  else()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -Wall")
  endif()
endif()


#---------------------------------------------------------------------------
# Include Directories
#
# In the Makefile these were:
#    -I. -I$(INSTALLDIR)/include
#---------------------------------------------------------------------------

1
#---------------------------------------------------------------------------
# Define the Executable Target
#
# The Makefile builds "AlbusIonosphere" from AlbusIonosphere.cxx.
#---------------------------------------------------------------------------
add_library(AlbusIonosphere SHARED AlbusIonosphere.cxx)

#---------------------------------------------------------------------------
# External Libraries
#
# The Makefile links against:
#   -lmim -lpim -liri -ljmavex -lvexplus -lvex -lsofa -lfl -lg2c -lm
#
# Adjust these as needed; CMake expects the libraries to be found via the linker.
#---------------------------------------------------------------------------
target_link_libraries(AlbusIonosphere
  mim
  pim
  iri
  jmavex
  vexplus
  vex
  sofa
  fl
  g2c
  m
)

#---------------------------------------------------------------------------
# Installation
#
# Install the executable into the "bin" subdirectory of the installation directory.
#---------------------------------------------------------------------------
install(TARGETS AlbusIonosphere
         DESTINATION ${INSTALLDIR}/bin
)

#---------------------------------------------------------------------------
# Custom Targets
#
# "clean" is automatically provided by the generated build system.
#
# Here is an example custom "distclean" target that removes all build artifacts.
# (Adjust as needed; note that CMake’s out-of-source build is recommended.)
#---------------------------------------------------------------------------
add_custom_target(distclean
    COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_BINARY_DIR}
    COMMENT "Removing all build artifacts"
)

