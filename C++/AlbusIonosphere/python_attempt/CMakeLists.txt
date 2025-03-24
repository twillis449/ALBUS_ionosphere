cmake_minimum_required(VERSION 3.10)
project(AlbusIonosphere CXX)

#---------------------------------------------------------------------------
# Source File(s)
#---------------------------------------------------------------------------
set(SRCS AlbusIonosphere.cxx)

#---------------------------------------------------------------------------
# Custom Target for "AlbusIonosphere"
#
# The original Makefile target "AlbusIonosphere" (which all other targets
# depend on) does not build an executable—it simply echoes a message.
# Here we create a custom target that always prints the message.
#---------------------------------------------------------------------------
add_custom_target(AlbusIonosphere ALL
    COMMAND ${CMAKE_COMMAND} -E echo "Nothing to do here, skipping."
    SOURCES ${SRCS}
)

#---------------------------------------------------------------------------
# (Optional) You could also add the source file to an executable target
# if you intend to compile it. For example:
#
# add_executable(AlbusIonosphereExe ${SRCS})
#
# However, to mimic the Makefile exactly, we only create the dummy target.
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Configuration Variables for Installation via Python setup.py
#
# These variables mimic the Makefile's environment variables.
# They can be set from the command line or via a CMake GUI.
#---------------------------------------------------------------------------
if(NOT DEFINED INSTALLDIR)
  set(INSTALLDIR "/usr/local" CACHE PATH "Installation directory")
endif()
if(NOT DEFINED VER_MAJOR)
  set(VER_MAJOR "1" CACHE STRING "Major version")
endif()
if(NOT DEFINED VER_MINOR)
  set(VER_MINOR "0" CACHE STRING "Minor version")
endif()
if(NOT DEFINED CURRENT_PYTHON)
  set(CURRENT_PYTHON "python" CACHE STRING "Python interpreter")
endif()

#---------------------------------------------------------------------------
# Custom "install" Target
#
# Instead of installing build artifacts directly, the original Makefile
# pipes version information into a Python setup script.
# Here we create a custom target "custom_install" that runs the command.
#
# (Note: We avoid naming it "install" because CMake reserves that name.)
#---------------------------------------------------------------------------
add_custom_target(custom_install
    COMMAND ${CMAKE_COMMAND} -E echo "${INSTALLDIR} ${VER_MAJOR} ${VER_MINOR}" 
            | ${CURRENT_PYTHON} setup.py build install --prefix=${INSTALLDIR} --install-platlib=${INSTALLDIR}/lib --install-purelib=${INSTALLDIR}/share/python
    COMMENT "Running Python setup.py build install"
)

#---------------------------------------------------------------------------
# Custom "distclean" Target
#
# CMake already provides a built-in "clean" target, but we add a custom
# "distclean" target that also removes the "build" directory and temporary files.
#---------------------------------------------------------------------------
add_custom_target(distclean
    COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_BINARY_DIR}/build
    COMMAND ${CMAKE_COMMAND} -E echo "Note: *~ files are not removed automatically."
    COMMENT "Performing additional cleaning (distclean)"
)

#---------------------------------------------------------------------------
# Note on Dependency Generation
#
# The original Makefile provides a "depend" target and custom rules for compiling
# .cxx files. In CMake, dependency handling and compilation rules are managed
# automatically.
#---------------------------------------------------------------------------

