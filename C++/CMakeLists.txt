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


## vex_to_sky2 actually depends on mim


