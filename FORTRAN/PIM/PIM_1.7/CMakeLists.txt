cmake_minimum_required(VERSION 3.10)
project(IRI_Project LANGUAGES Fortran C CXX)

#--------------------------------------------------------------------------
# Version numbers and installation directory
#--------------------------------------------------------------------------
set(VER_MAJOR 1)
set(VER_MINOR 0)
set(VER_SUBSUB 0)

if(NOT DEFINED INSTALLDIR)
  set(INSTALLDIR ${CMAKE_INSTALL_PREFIX} CACHE PATH "Installation directory")
endif()

#--------------------------------------------------------------------------
# Headers to install
#--------------------------------------------------------------------------
set(IRI_HEADERS jma_irisub_c.h)

#--------------------------------------------------------------------------
# Source files
#
# Group A & B (OBJS + NOBJS) used for building the library:
#   OBJS:   cira.f, igrf.f, iridreg.f, irifun.f
#   NOBJS:  irisub.f, iritec.f, jma_irisub.F, jma_irisub_c.c
#--------------------------------------------------------------------------
set(IRI_LIBRARY_SOURCES
  cira.f
  igrf.f
  iridreg.f
  irifun.f
  irisub.f
  iritec.f
  jma_irisub.F
  jma_irisub_c.c
)

# Group C (NEOBJS) used for the "iritestne" executable:
set(IRI_NE_SOURCES
  irisubne.f
  iritecne.f
)

#--------------------------------------------------------------------------
# Library Targets
#
# Create a shared library "iri" (libiri.so) with versioning.
# Also create a static library "iri_static" if needed.
#--------------------------------------------------------------------------
add_library(iri SHARED ${IRI_LIBRARY_SOURCES})
set_target_properties(iri PROPERTIES
  OUTPUT_NAME "iri"
  VERSION "${VER_MAJOR}.${VER_MINOR}.${VER_SUBSUB}"
  SOVERSION "${VER_MAJOR}"
)
# Link with math library if required.
target_link_libraries(iri PRIVATE m)

# Optional: create a static version (libiri.a)
add_library(iri_static STATIC ${IRI_LIBRARY_SOURCES})
set_target_properties(iri_static PROPERTIES OUTPUT_NAME "iri")

# Create a static library for the extra sources (NEOBJS)
add_library(iri_ne STATIC ${IRI_NE_SOURCES})
set_target_properties(iri_ne PROPERTIES OUTPUT_NAME "iri_ne")

#--------------------------------------------------------------------------
# Executable Targets
#
# It is assumed that the following source files exist:
#   - iritest.f        : For "iritest"
#   - iritestne.f      : For "iritestne"
#   - jma_iri.f        : For "jma_iri"
#   - make_ap.cxx      : For "make_ap"
#--------------------------------------------------------------------------
add_executable(iritest iritest.f)
target_link_libraries(iritest PRIVATE iri)

add_executable(iritestne iritestne.f)
target_link_libraries(iritestne PRIVATE iri iri_ne)

add_executable(jma_iri jma_iri.f)
target_link_libraries(jma_iri PRIVATE iri)

add_executable(make_ap make_ap.cxx)
# (Link additional libraries if needed)

#--------------------------------------------------------------------------
# Installation
#
# Install the shared library, header file, and data files (*.dat and *.asc).
#--------------------------------------------------------------------------
install(TARGETS iri
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)
install(FILES ${IRI_HEADERS} DESTINATION include)
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        FILES_MATCHING PATTERN "*.dat" PATTERN "*.asc"
        DESTINATION libdata/IRI)

#--------------------------------------------------------------------------
# Notes:
#
# - Dependency generation, clean, and distclean targets are handled automatically
#   by CMake and the underlying build system.
# - Adjust file names and target link libraries as needed.
#--------------------------------------------------------------------------

