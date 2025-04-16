cmake_minimum_required(VERSION 3.10)
project(JMA_code)

# --------------------------------------------------------------------
# Configure Converter settings
# --------------------------------------------------------------------
set(CONVERTER "mlftounf")

# Determine the converter input extension based on F77_RECL_UNIT.
# You can specify F77_RECL_UNIT when configuring CMake (e.g. -DF77_RECL_UNIT=bytes).
if(NOT DEFINED F77_RECL_UNIT)
  set(F77_RECL_UNIT "bytes")
endif()

if(F77_RECL_UNIT STREQUAL "bytes")
  set(CONVERTER_INPUT_EXT "in")
else()
  set(CONVERTER_INPUT_EXT "inw")
endif()

# Use INSTALLDIR if provided; otherwise default to CMAKE_INSTALL_PREFIX.
if(NOT DEFINED INSTALLDIR)
  set(INSTALLDIR "${CMAKE_INSTALL_PREFIX}")
endif()

# --------------------------------------------------------------------
# Target: convert
#
# Equivalent to:
#   all: convert
#   convert: $(MAKE) -C utils $(CONVERTER)
# --------------------------------------------------------------------
# --------------------------------------------------------------------
# Target: custom_install
#
# Mimics the following Makefile install steps:
#
# 1. Ensure utils/$(CONVERTER) is built (dependency on convert)
# 2. Copy utils/$(CONVERTER) into the "form" directory
# 3. Change into form, run the converter with input from ../utils/$(CONVERTER).$(CONVERTER_INPUT_EXT)
# 4. Create the directory $(INSTALLDIR)/libdata/PIM/mlfdb/unform
# 5. Move files matching nm*.* and sm*.* from form to that directory
# 6. Touch the file "install"
#
# Note: The target is named "custom_install" to avoid conflict with CMake's built-in install.
# --------------------------------------------------------------------

# --------------------------------------------------------------------
# Target: depend
#
# Mimics dependency generation. The original Makefile uses a loop
# to generate dependency files; here we simply print a message.
# --------------------------------------------------------------------

# --------------------------------------------------------------------
# Target: my_clean
#
# Removes generated files similar to the Makefile's "clean" target.
# --------------------------------------------------------------------
# --------------------------------------------------------------------
# Target: distclean
#
# Extends cleaning by also removing backup files matching "*~".
#
