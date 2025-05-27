cmake_minimum_required(VERSION 3.10)
project(JMA_Code)

# --------------------------------------------------------------------
# Set lists corresponding to your Makefile directories
# --------------------------------------------------------------------
set(DATADIRS
  cgmdb
  llfdb
  lmedb
  mlfdb
  noaa_dat
  ursidb
  usudb
)

set(PIMDIR pim/source/vlb4)

# --------------------------------------------------------------------
# Add subdirectories
# --------------------------------------------------------------------
# For each data directory, add it as a subdirectory.
# It is assumed that each of these subdirs contains its own CMakeLists.txt.
foreach(dir ${DATADIRS})
  add_subdirectory(${dir})
endforeach()

# Add the PIM subdirectory
add_subdirectory(${PIMDIR})

# --------------------------------------------------------------------
# Custom targets to mimic original Makefile actions
# --------------------------------------------------------------------
# The Makefile defines a “subdirs” target that builds and installs in each subdir.
# Here we create custom targets that call the equivalent commands.

# For each data directory, create custom targets for convert and install.
foreach(dir ${DATADIRS})
  add_custom_target(${dir}_convert
    COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_SOURCE_DIR}/${dir} ${CMAKE_MAKE_PROGRAM} convert
    COMMENT "Running convert in ${dir}"
  )
  add_custom_target(${dir}_install
    COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_SOURCE_DIR}/${dir} ${CMAKE_MAKE_PROGRAM} install
    COMMENT "Running install in ${dir}"
  )
endforeach()

# For the PIM directory, note that it depends on the data directories.
add_custom_target(pim_build
  COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_SOURCE_DIR}/${PIMDIR} ${CMAKE_MAKE_PROGRAM}
  COMMENT "Building in ${PIMDIR}"
)

add_custom_target(pim_install
  COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_SOURCE_DIR}/${PIMDIR} ${CMAKE_MAKE_PROGRAM} install
  COMMENT "Running install in ${PIMDIR}"
)

# A combined target that depends on all subdirectory targets.
add_custom_target(subdirs
  DEPENDS
    ${DATADIRS}_convert
    ${DATADIRS}_install
    pim_build
    pim_install
  COMMENT "Running all subdir targets"
)
# Note: In the above DEPENDS, CMake does not automatically expand list names.
# You may need to manually add each dependency or generate the list.
# One workaround is to define variables with all convert and install targets.
set(ALL_DATADIRS_CONVERT "")
set(ALL_DATADIRS_INSTALL "")
foreach(dir ${DATADIRS})
  list(APPEND ALL_DATADIRS_CONVERT ${dir}_convert)
  list(APPEND ALL_DATADIRS_INSTALL ${dir}_install)
endforeach()
add_custom_target(subdirs_fixed
  DEPENDS ${ALL_DATADIRS_CONVERT} ${ALL_DATADIRS_INSTALL} pim_build pim_install
  COMMENT "Running all subdir targets"
)

# --------------------------------------------------------------------
# Additional targets (depend, clean, distclean, install)
# --------------------------------------------------------------------
# The "depend" target – here we simply print a message.
add_custom_target(depend
  COMMAND ${CMAKE_COMMAND} -E echo "Running depend target... (custom dependency generation would go here)"
  COMMENT "Generating dependencies"
)

# Note: The "clean" target is provided by the build system (e.g. 'make clean')
# The "distclean" target can be added as a custom target if needed.
add_custom_target(PIM_distclean
  COMMAND ${CMAKE_COMMAND} -E echo "Performing distclean"
  # You can add commands to remove extra files here (e.g. using file(REMOVE ...))
  COMMENT "Cleaning and removing extra files"
)

# Custom install steps:
# Create the install directory $(INSTALLDIR)/libdata/PIM.
# If INSTALLDIR is not defined, default to CMAKE_INSTALL_PREFIX.
if(NOT DEFINED INSTALLDIR)
  set(INSTALLDIR ${CMAKE_INSTALL_PREFIX})
endif()

install(CODE "file(MAKE_DIRECTORY \"${INSTALLDIR}/libdata/PIM\")"
  COMMENT "Creating directory ${INSTALLDIR}/libdata/PIM for data installation"
)

# --------------------------------------------------------------------
# Additional rules for compiling C/C++ and Fortran files may be defined
# below if needed. In CMake these are usually set via variables such as:
#   set(CMAKE_C_COMPILER ...)
#   set(CMAKE_Fortran_COMPILER ...)
#
# Compiler flags (CFLAGS, FFLAGS) can be set similarly.
#
# For example:
#   set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} <your-flags>")
#   set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} <your-flags>")