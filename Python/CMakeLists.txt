cmake_minimum_required(VERSION 3.10)
project(JMA_code)

# List of subdirectories
set(SUBDIRS python_scripts)

# Add each subdirectory.
# Each subdirectory is expected to have its own CMakeLists.txt that defines its targets
foreach(python_subdir ${SUBDIRS})
  add_subdirectory(${python_subdir})
endforeach()

# --------------------------------------------------------------------
# Custom Target: subdirs
#
# This target mimics the Makefile rule that enters each subdirectory
# and builds and installs its content.
#
# In CMake, building and installing is normally handled by the individual
# subdirectories' CMakeLists.txt files. Here we create a custom target that
# depends on all subdirectories (if they define a target with the same name as the directory).
# --------------------------------------------------------------------
add_custom_target(python_subdirs
  COMMENT "Building all subdirectories"
)
# For each subdirectory, we assume that the subdirectory's CMakeLists.txt defines
# a target with the same name as the subdirectory. Adjust if necessary.
#foreach(python_subdir ${SUBDIRS})
#  add_dependencies(python_subdirs ${python_subdir})
#endforeach()

# --------------------------------------------------------------------
# Custom Targets for dependency generation, cleaning, distclean, and install
#
# These targets mimic the original Makefile but in CMake you normally rely on
# CMake's own clean target and the install() commands provided in each subdirectory.
# Here we add placeholder targets which can be extended as needed.
# --------------------------------------------------------------------
add_custom_target(python_depend
  COMMAND ${CMAKE_COMMAND} -E echo "Running dependency generation in subdirectories (customize as needed)"
  COMMENT "Generating dependencies in subdirectories"
)

add_custom_target(python_clean
  COMMAND ${CMAKE_COMMAND} -E echo "Cleaning subdirectories (use 'cmake --build . --target clean' for build clean)"
  COMMENT "Cleaning subdirectories"
)

add_custom_target(python_distclean
  COMMAND ${CMAKE_COMMAND} -E echo "Performing distclean in subdirectories (customize removal of backup files)"
  COMMENT "Distcleaning subdirectories"
)
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/python_scripts/
        DESTINATION share/python
        FILES_MATCHING PATTERN "*.py")

# The install process in CMake is handled via the install() commands in each subdirectory.
# If you want a custom target that calls the install rules defined in the subdirectories,
# you can do so. (Here we name it custom_install to avoid conflict with CMake's built-in install.)
add_custom_target(python_install
  COMMAND ${CMAKE_COMMAND} -E echo "Installing subdirectories (run 'cmake --build . --target install' to install)"
  COMMENT "Installing subdirectories"
)

# --------------------------------------------------------------------
# Notes:
#
# - In the original Makefile, the subdirectories were built by running "make" and then "make install"
#   in each directory. With CMake, it is preferred to define targets in each subdirectory via add_subdirectory().
#
# - The custom targets defined above (depend, my_clean, my_distclean, custom_install) are simple placeholders.
#   For actual cleaning or dependency generation, you may need to add more specific commands.
#
# - CMake automatically handles source file compilation rules so there is no need to define suffix rules.
# --------------------------------------------------------------------
