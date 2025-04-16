# Install script for directory: /home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

# Set default install directory permissions.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/usr/bin/objdump")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/libmim.a")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE FILE FILES
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/3C.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/GPS.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/GPS_collection.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/GPS_criteria.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/GPS_receiver.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/GPS_receiver_obs.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/GPS_satellites.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/GPS_times.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/MC_direction.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/MC_random.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/MT_random.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/VTEC.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/atmosphere_point.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/cal_source.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/data_calibrator.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/filter.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/iono_runner.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/ionosphere.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/ionosphere_fake.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/ionosphere_gen.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/ionosphere_gps.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/ionosphere_iri.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/ionosphere_iriplus.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/ionosphere_pim.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/latlon_cart.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/linfit.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/linfit_grad.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/location.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/observation.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/observation_3D.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/physical_constants.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/pim_runner.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/random_source.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/sofa_helpers.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/space_rotation_matrix.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/space_unit_vector.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/space_vector.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/station_latlon.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/station_maker.h"
    "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/station_reference.h"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/lofar")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar"
         OLD_RPATH "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/lib:/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vexplus:"
         NEW_RPATH "")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar")
    endif()
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar2" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar2")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar2"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/lofar2")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar2" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar2")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar2"
         OLD_RPATH "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/lib:/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vexplus:"
         NEW_RPATH "")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar2")
    endif()
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar3" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar3")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar3"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/lofar3")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar3" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar3")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar3"
         OLD_RPATH "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/lib:/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vexplus:"
         NEW_RPATH "")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar3")
    endif()
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar4" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar4")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar4"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/lofar4")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar4" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar4")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar4"
         OLD_RPATH "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/lib:/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vexplus:"
         NEW_RPATH "")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar4")
    endif()
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar5" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar5")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar5"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/lofar5")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar5" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar5")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar5"
         OLD_RPATH "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/lib:/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vexplus:"
         NEW_RPATH "")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar5")
    endif()
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar6" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar6")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar6"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/lofar6")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar6" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar6")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar6"
         OLD_RPATH "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/lib:/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vexplus:"
         NEW_RPATH "")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar6")
    endif()
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar7" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar7")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar7"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/lofar7")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar7" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar7")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar7"
         OLD_RPATH "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/lib:/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vexplus:"
         NEW_RPATH "")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar7")
    endif()
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar8" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar8")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar8"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/lofar8")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar8" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar8")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar8"
         OLD_RPATH "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/lib:/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vexplus:"
         NEW_RPATH "")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar8")
    endif()
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar9" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar9")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar9"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/lofar9")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar9" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar9")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar9"
         OLD_RPATH "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/lib:/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vexplus:"
         NEW_RPATH "")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar9")
    endif()
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar10" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar10")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar10"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/lofar10")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar10" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar10")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar10"
         OLD_RPATH "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/lib:/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vexplus:"
         NEW_RPATH "")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/lofar10")
    endif()
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/test_pim" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/test_pim")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/test_pim"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/test_pim")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/test_pim" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/test_pim")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/test_pim"
         OLD_RPATH "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/lib:/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vexplus:"
         NEW_RPATH "")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/test_pim")
    endif()
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/test2_pim" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/test2_pim")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/test2_pim"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/test2_pim")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/test2_pim" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/test2_pim")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/test2_pim"
         OLD_RPATH "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/lib:/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vexplus:"
         NEW_RPATH "")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/test2_pim")
    endif()
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/vlba_obs" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/vlba_obs")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/vlba_obs"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/mim/test/PIMrunner/vlba_obs")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/vlba_obs" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/vlba_obs")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/vlba_obs"
         OLD_RPATH "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/lib:/home/user/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vexplus:"
         NEW_RPATH "")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/vlba_obs")
    endif()
  endif()
endif()

