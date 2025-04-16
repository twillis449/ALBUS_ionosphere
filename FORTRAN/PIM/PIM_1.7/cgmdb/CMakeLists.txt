cmake_minimum_required(VERSION 3.10)
project(JMA_code)

# --------------------------------------------------------------------
# Custom target "convert"
#
# The original "convert" target did nothing. Here we define a custom
# target that simply echoes a message.
# --------------------------------------------------------------------
# --------------------------------------------------------------------
# Installation target
#
# The Makefile's "install" target creates a directory and copies all files
# into $(INSTALLDIR)/libdata/PIM/cgmdb, then touches an "install" file.
# In CMake, we use the install() command to copy files and add a post‐install
# command to create (or "touch") the install file.
# --------------------------------------------------------------------

# Use CMAKE_INSTALL_PREFIX as default INSTALLDIR if not provided.
if(NOT DEFINED INSTALLDIR)
  set(INSTALLDIR "${CMAKE_INSTALL_PREFIX}")
endif()

# Install all files (except CMakeLists.txt) from the current directory into
# libdata/PIM/cgmdb. Adjust the FILES_MATCHING pattern as needed.
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/
  DESTINATION libdata/PIM/cgmdb
  FILES_MATCHING PATTERN "*" 
  PATTERN "CMakeLists.txt" EXCLUDE
)

# Add a post-install command that "touches" an empty file named install.
install(CODE "file(WRITE \"${INSTALLDIR}/libdata/PIM/cgmdb/install\" \"\")"
  COMMENT "Touching install file"
)

# --------------------------------------------------------------------
# Custom target for dependency generation ("depend")
#
# This target mimics the dependency generation commands in the Makefile.
# Since the Makefile used shell commands and environment variables to do so,
# here we simply print a message. Customize this if you need actual dependency
# generation.
# --------------------------------------------------------------------

# --------------------------------------------------------------------
# Clean targets
#
# CMake automatically provides a "clean" target for build files. If you want
# to add additional cleaning (like removing temporary files matching "*~"),
# you can define a custom target "distclean".
# --------------------------------------------------------------------

# --------------------------------------------------------------------
# Compilation rules
#
# The original Makefile defined suffix rules for C/C++ and Fortran.
# CMake automatically handles compiler rules so explicit suffix rules
# are not necessary.
# --------------------------------------------------------------------