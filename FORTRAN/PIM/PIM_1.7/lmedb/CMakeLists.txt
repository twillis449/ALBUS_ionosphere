cmake_minimum_required(VERSION 3.10)
project(JMA_code)

# --------------------------------------------------------------------
# Configure Converter settings
# --------------------------------------------------------------------
set(CONVERTER "lmetounf")

# Determine the converter input extension based on F77_RECL_UNIT.
# You can pass F77_RECL_UNIT as an option to CMake; default is "bytes".
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
#add_custom_target(convert
#  COMMAND ${CMAKE_MAKE_PROGRAM} -C ${CMAKE_CURRENT_SOURCE_DIR}/utils ${CONVERTER}
#  COMMENT "Building converter (${CONVERTER}) in utils"
#)

# --------------------------------------------------------------------
# Target: custom_install
#
# Mimics the following Makefile install steps:
#   - Ensure utils/$(CONVERTER) exists (dependency on convert)
#   - Copy utils/$(CONVERTER) to form directory
#   - In form, run: ./$(CONVERTER) < ../utils/$(CONVERTER).$(CONVERTER_INPUT_EXT)
#   - Create $(INSTALLDIR)/libdata/PIM/lmedb/unform
#   - Move files matching lm*.* from form to the install directory
#   - Touch file "install"
#
# Note: The target is named "custom_install" to avoid conflict with CMake's install().
# --------------------------------------------------------------------
#add_custom_target(custom_install
#  DEPENDS convert
#  COMMAND ${CMAKE_COMMAND} -E copy
#          ${CMAKE_CURRENT_SOURCE_DIR}/utils/${CONVERTER}
 #         ${CMAKE_CURRENT_SOURCE_DIR}/form
 # COMMAND ${CMAKE_COMMAND} -E echo "Running converter in form"
 # COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_SOURCE_DIR}/form ${CMAKE_COMMAND} -E env sh -c "./${CONVERTER} < ../utils/${CONVERTER}.${CONVERTER_INPUT_EXT}"
 # COMMAND ${CMAKE_COMMAND} -E make_directory ${INSTALLDIR}/libdata/PIM/lmedb/unform
  # CMake's -E rename does not support wildcards, so we use a shell command.
 # COMMAND sh -c "mv ${CMAKE_CURRENT_SOURCE_DIR}/form/lm*.* ${INSTALLDIR}/libdata/PIM/lmedb/unform"
 # COMMAND ${CMAKE_COMMAND} -E touch ${CMAKE_CURRENT_SOURCE_DIR}/install
 # COMMENT "Performing custom install steps"
#)

# --------------------------------------------------------------------
# Target: depend
#
# Mimics dependency generation (placeholder).
# --------------------------------------------------------------------
#add_custom_target(depend
 # COMMAND ${CMAKE_COMMAND} -E echo "Running dependency generation (customize as needed)"
 # COMMENT "Generating dependencies"
#)

# --------------------------------------------------------------------
# Target: my_clean
#
# Removes generated files similar to the Makefile's "clean" target.
# --------------------------------------------------------------------
#add_custom_target(my_clean
#  COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_SOURCE_DIR}/form/${CONVERTER}
#  COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_SOURCE_DIR}/utils/${CONVERTER}
#  COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_SOURCE_DIR}/utils/*.o
#  COMMAND sh -c "rm -f ${CMAKE_CURRENT_SOURCE_DIR}/form/lm*.*"
#  COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_SOURCE_DIR}/install
#  COMMENT "Cleaning generated files"
#)

# --------------------------------------------------------------------
# Target: distclean
#
# Extends cleaning by removing backup files matching "*~".
# --------------------------------------------------------------------
#add_custom_target(distclean
#  DEPENDS my_clean
#  COMMAND sh -c "rm -f ${CMAKE_CURRENT_SOURCE_DIR}/*~"
#  COMMENT "Performing distclean: removing backup files"
#)

# --------------------------------------------------------------------
# Note:
# - CMake automatically provides its own clean target to remove build artifacts.
# - Compilation rules for .cxx, .F, and .f files are handled by CMake automatically.
# --------------------------------------------------------------------
