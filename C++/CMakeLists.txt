cmake_minimum_required(VERSION 3.10)
project(JMA_Code)

#----------------------------------------------------------------------------
# Add subdirectories in an order reflecting the dependencies:
#
#   vex_to_sky2  ---+
#                   |--> mim/test/PIMrunner  ---+
#                                                |--> AlbusIonosphere/python_attempt
#----------------------------------------------------------------------------
# Note: Each subdirectory must have its own CMakeLists.txt.
add_subdirectory(vex_to_sky2)
add_subdirectory(mim/test/PIMrunner)
add_subdirectory(AlbusIonosphere/python_attempt)

#----------------------------------------------------------------------------
# Set up dependencies between targets.
#
# In the original Makefile:
#   - mim/test/PIMrunner depends on vex_to_sky2
#   - AlbusIonosphere/python_attempt depends on both vex_to_sky2 and mim/test/PIMrunner
#
# For this example, assume each subdirectory defines a target:
#   - "vex_to_sky2_target" in vex_to_sky2
#   - "PIMrunner_target" in mim/test/PIMrunner
#   - "python_attempt_target" in AlbusIonosphere/python_attempt
#
# Adjust these target names to match what is defined in the subdirectories.
add_dependencies(PIMrunner_target vex_to_sky2_target)
add_dependencies(python_attempt_target vex_to_sky2_target PIMrunner_target)

#----------------------------------------------------------------------------
# Optionally, define a custom target to build all subdirectories
# (this is analogous to the "all" and "subdirs" targets in the Makefile).
add_custom_target(subdirs ALL
  DEPENDS vex_to_sky2_target PIMrunner_target python_attempt_target
)

#----------------------------------------------------------------------------
# Installation:
#
# In CMake the install() commands should be placed inside each subdirectory's
# CMakeLists.txt so that "make install" (or "cmake --build . --target install")
# will invoke the appropriate install rules. This top-level file does not need
# to duplicate those commands.

