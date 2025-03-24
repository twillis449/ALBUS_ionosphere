cmake_minimum_required(VERSION 3.10)
project(vex LANGUAGES CXX C)

#---------------------------------------------------------------------------
# Enable Position Independent Code (i.e. -fPIC) globally
#---------------------------------------------------------------------------
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

#---------------------------------------------------------------------------
# Find Bison and Flex (for parser and scanner generation)
#---------------------------------------------------------------------------
find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)

# Process the Bison file (vex.y)
BISON_TARGET(Parser vex.y
  ${CMAKE_CURRENT_BINARY_DIR}/y.tab.c
  DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/y.tab.h
  COMMAND_VAR BISON_COMMAND
  )
# Process the Flex file (vex.yy.l)
FLEX_TARGET(Scanner vex.yy.l
  ${CMAKE_CURRENT_BINARY_DIR}/lex.yy.c
  )
# Ensure that Flex is run after Bison so that the scanner sees y.tab.h
ADD_FLEX_BISON_DEPENDENCY(Scanner Parser)

#---------------------------------------------------------------------------
# Specify source and header files
#
# Here we assume that your hand‐written sources are:
#   vex.cxx, vex_util.cxx, print_vex.cxx, vex_get.cxx
#
# In addition, the generated files from Bison and Flex will be compiled.
#---------------------------------------------------------------------------
set(SRC_FILES
    vex.cxx
    ${BISON_Parser_OUTPUTS}    # This expands to y.tab.c (y.tab.h is generated as well)
    ${FLEX_Scanner_OUTPUTS}     # This expands to lex.yy.c (your scanner)
    vex_util.cxx
    print_vex.cxx
    vex_get.cxx
)

# List headers to be installed (note that y.tab.h is generated)
set(HEADERS
    vex++.h
    vex.h
    ${CMAKE_CURRENT_BINARY_DIR}/y.tab.h
)

#---------------------------------------------------------------------------
# Create the library targets
#
# In the original Makefile, several targets were provided:
#  - libvex.a  (static library)
#  - libvex.sl (shared library built via gcc -shared)
#  - libvex.so (shared library with versioning)
#
# Here we create both a shared library and a static library.
#---------------------------------------------------------------------------
add_library(vex_shared SHARED ${SRC_FILES})
set_target_properties(vex_shared PROPERTIES OUTPUT_NAME "vex"
    VERSION "1.0.0" SOVERSION "1"
)

add_library(vex_static STATIC ${SRC_FILES})
set_target_properties(vex_static PROPERTIES OUTPUT_NAME "vex")

#---------------------------------------------------------------------------
# Installation
#
# This mimics the install target by copying the shared library and headers
# into the specified installation directories.
#
# Note: You can override the installation prefix by setting CMAKE_INSTALL_PREFIX.
#---------------------------------------------------------------------------
install(TARGETS vex_shared vex_static
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
)

install(FILES ${HEADERS} DESTINATION include)

#---------------------------------------------------------------------------
# Custom targets for cleaning are provided by the generated build system.
#---------------------------------------------------------------------------

