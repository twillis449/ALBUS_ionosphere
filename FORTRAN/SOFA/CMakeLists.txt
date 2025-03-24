cmake_minimum_required(VERSION 3.10)
project(JMA_code)

#---------------------------------------------------------------------------
# Add subdirectories
#
# The Makefile recurses into the "sofa" directory.
# Each subdirectory is expected to have its own CMakeLists.txt.
#---------------------------------------------------------------------------
add_subdirectory(sofa)

#---------------------------------------------------------------------------
# Custom target for installing headers from the sofa subdirectory
#
# This replicates the "sofa_headers" rule in the Makefile.
# It assumes that the sofa CMakeLists.txt defines a target named "install_headers".
# If not, adjust the command as needed.
#---------------------------------------------------------------------------
add_custom_target(sofa_headers
    COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR}/sofa --target install_headers
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/sofa
    COMMENT "Running install_headers in the sofa subdirectory"
)

#---------------------------------------------------------------------------
# Note on Other Targets
#
# - The "all" target is automatically provided by CMake (it builds all added targets).
# - CMake automatically provides a "clean" target.
# - Additional targets like "depend" or "distclean" are typically not needed
#   with CMake’s out-of-source builds.
# - To perform installation, invoke "cmake --build . --target install" after configuration.
#---------------------------------------------------------------------------

