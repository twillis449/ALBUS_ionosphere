cmake_minimum_required(VERSION 3.10)
project(JMA_code)

#----------------------------------------------------------------------------
# Add Subdirectories
#
# The original Makefile recurses into the subdirectories:
#   SOFA, IRI, and PIM/PIM_1.7.
#
# In CMake each subdirectory should contain its own CMakeLists.txt.
#----------------------------------------------------------------------------
add_subdirectory(SOFA)
add_subdirectory(IRI)
add_subdirectory(PIM/PIM_1.7)

#----------------------------------------------------------------------------
# Optional: Specify Dependencies Between Subdirectories
#
# The Makefile indicates that "PIM/PIM_1.7" depends on "SOFA". If, for example,
# the SOFA subdirectory defines a target named "SOFA_target" and the PIM/PIM_1.7
# subdirectory defines a target "PIM_target", you can enforce the dependency as:
#
#    add_dependencies(PIM_target SOFA_target)
#
# Adjust target names as needed.
#----------------------------------------------------------------------------
# Example (uncomment and adjust if applicable):
# add_dependencies(PIM_target SOFA_target)

#----------------------------------------------------------------------------
# Note on Other Targets
#
# - The default "all" target in CMake will build all targets added via add_subdirectory.
# - CMake automatically provides "clean" functionality.
# - The "install" target will invoke install() commands from the subdirectories if they
#   are defined.
#
# There is no need to manually re-create phony targets like "depend", "clean", or "distclean".
#----------------------------------------------------------------------------

