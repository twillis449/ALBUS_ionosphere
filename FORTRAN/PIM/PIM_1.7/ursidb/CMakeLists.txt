cmake_minimum_required(VERSION 3.10)
project(JMA_code)

# Use INSTALLDIR if provided; otherwise default to CMAKE_INSTALL_PREFIX.
if(NOT DEFINED INSTALLDIR)
  set(INSTALLDIR "${CMAKE_INSTALL_PREFIX}")
endif()

# --------------------------------------------------------------------
# Target: convert
#
# The original "convert" target is empty. Here we simply echo a message.
# --------------------------------------------------------------------

# --------------------------------------------------------------------
# Target: custom_install
#
# This target performs the following steps:
#   1. Creates the install directory: $(INSTALLDIR)/libdata/PIM/ursidb
#   2. Copies all files from the current source directory to that directory.
#   3. Touches (creates or updates the timestamp of) a file named "install" in the source directory.
#
# Note: We name this target "custom_install" to avoid conflict with CMake’s built-in install() command.
# --------------------------------------------------------------------

# --------------------------------------------------------------------
# Target: depend
#
# The dependency target is a placeholder since the original rule uses macros for dependency generation.
# --------------------------------------------------------------------

# --------------------------------------------------------------------
# Target: my_clean
#
# This target mimics the "clean" target by removing generated files.
# Here we remove the "install" file. You can add additional cleanup commands as needed.
# --------------------------------------------------------------------

# --------------------------------------------------------------------
# Target: distclean
#
# This target extends "my_clean" by also removing backup files matching "*~".
# --------------------------------------------------------------------
# --------------------------------------------------------------------
# Note on Compilation Rules
#
# The original Makefile also includes suffix rules for C/C++ and Fortran.
# CMake manages these automatically, so explicit rules are not necessary.
# --------------------------------------------------------------------

