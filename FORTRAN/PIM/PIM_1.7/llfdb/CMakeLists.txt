cmake_minimum_required(VERSION 3.10)
project(JMA_code)

# --------------------------------------------------------------------
# Configure Converter settings
# --------------------------------------------------------------------
# Set the converter program name
set(CONVERTER "llftounf")

# Configure the input file extension based on F77_RECL_UNIT.
# You can define F77_RECL_UNIT when configuring CMake (e.g. -DF77_RECL_UNIT=bytes)
if(NOT DEFINED F77_RECL_UNIT)
  # Provide a default if not specified
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
# This target simply changes directory to "utils" and builds the converter.
# Equivalent to: $(MAKE) -C utils $(CONVERTER)
# --------------------------------------------------------------------
#add_custom_target(convert
 # COMMAND ${CMAKE_MAKE_PROGRAM} -C ${CMAKE_CURRENT_SOURCE_DIR}/utils ${CONVERTER}
 # COMMENT "Running convert target: building converter in utils"
#)

# --------------------------------------------------------------------
# Target: install (custom)
#
# This custom target does the following:
#   1. Ensures the converter binary is built (depends on utils/${CONVERTER}).
#   2. Copies the converter from utils to the local "form" directory.
#   3. Changes to the "form" directory and runs the converter with input from
#      ../utils/${CONVERTER}.${CONVERTER_INPUT_EXT}.
#   4. Creates the install directory ${INSTALLDIR}/libdata/PIM/llfdb/unform.
#   5. Moves files (brz*.*, ind*.*, pac*.*, usa*.*) from "form" into that directory.
#   6. Touches (creates or updates the timestamp of) a file named "install".
#
# Note: This target uses shell commands (e.g. cp, cd, mv) so it assumes a POSIX-like shell.
# --------------------------------------------------------------------
#add_custom_target(custom_install
#  DEPENDS convert
 # COMMAND ${CMAKE_COMMAND} -E echo "Copying converter from utils to form"
 # COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/utils/${CONVERTER} ${CMAKE_CURRENT_SOURCE_DIR}/form
 # COMMAND ${CMAKE_COMMAND} -E echo "Running converter in form"
 # COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_SOURCE_DIR}/form ${CMAKE_COMMAND} -E env sh -c "./${CONVERTER} < ../utils/${CONVERTER}.${CONVERTER_INPUT_EXT}"
 # COMMAND ${CMAKE_COMMAND} -E make_directory ${INSTALLDIR}/libdata/PIM/llfdb/unform
  # Move files with patterns. CMake's -E rename doesn't support wildcards so we invoke a shell.
  #COMMAND ${CMAKE_COMMAND} -E echo "Moving output files to install directory"
  #COMMAND sh -c "mv ${CMAKE_CURRENT_SOURCE_DIR}/form/brz*.* ${CMAKE_CURRENT_SOURCE_DIR}/form/ind*.* ${CMAKE_CURRENT_SOURCE_DIR}/form/pac*.* ${CMAKE_CURRENT_SOURCE_DIR}/form/usa*.* ${INSTALLDIR}/libdata/PIM/llfdb/unform"
  #COMMAND ${CMAKE_COMMAND} -E touch ${CMAKE_CURRENT_SOURCE_DIR}/install
  #COMMENT "Performing custom install steps"
#)

# --------------------------------------------------------------------
# Target: depend
#
# Mimics dependency generation. Actual dependency generation would require
# further customization.
# --------------------------------------------------------------------
#add_custom_target(depend
#  COMMAND ${CMAKE_COMMAND} -E echo "Running dependency generation (customize as needed)"
#  COMMENT "Generating dependencies"
#)

# --------------------------------------------------------------------
# Target: clean
#
# Removes files generated during build/install.
# Note: CMake generators provide a 'clean' target for build artifacts,
# but here we remove extra files like the converter copies and moved files.
# --------------------------------------------------------------------
#add_custom_target(my_clean
#  COMMAND ${CMAKE_COMMAND} -E echo "Removing generated files..."
  # Remove local form copy of the converter.
 # COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_SOURCE_DIR}/form/${CONVERTER}
  # Remove moved files in form (using shell wildcard expansion)
  #COMMAND sh -c "rm -f ${CMAKE_CURRENT_SOURCE_DIR}/form/brz*.* ${CMAKE_CURRENT_SOURCE_DIR}/form/ind*.* ${CMAKE_CURRENT_SOURCE_DIR}/form/pac*.* ${CMAKE_CURRENT_SOURCE_DIR}/form/usa*.*"
  # Remove converter binary in utils and any object files
  #COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_SOURCE_DIR}/utils/${CONVERTER}
  #COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_CURRENT_SOURCE_DIR}/utils/CMakeFiles
  # Remove the "install" file
  #COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_SOURCE_DIR}/install
  #COMMENT "Cleaning up extra files"
#)

# --------------------------------------------------------------------
# Target: distclean
#
# Extends "clean" by removing backup files matching "*~".
# --------------------------------------------------------------------
#add_custom_target(distclean
#  DEPENDS my_clean
#  COMMAND sh -c "rm -f ${CMAKE_CURRENT_SOURCE_DIR}/*~"
#  COMMENT "Performing distclean: cleaning up backup files"
#)

# --------------------------------------------------------------------
# Additional notes:
#
# - The original suffix rules for .cxx, .F, and .f files are managed by CMake's
#   built-in rules for C/C++ and Fortran compilers. No explicit rule is needed.
#
# - The provided commands assume a POSIX shell environment. If you are building on a
#   non-POSIX system (e.g. Windows), you'll need to adjust the commands accordingly.
#
# - The 'install' target in this file is named 'custom_install' to avoid conflict with
#   CMake's built-in install() command. Invoke it with: make custom_install
# --------------------------------------------------------------------
