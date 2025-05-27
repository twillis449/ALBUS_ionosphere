cmake_minimum_required(VERSION 3.10)
project(JMA_code Fortran C)

# Enable the necessary languages.
enable_language(Fortran)
enable_language(C)

# --------------------------------------------------------------------
# Define header and source files
# --------------------------------------------------------------------
set(HEADERS
  pim_asub_c.h
)

# These sources (from your OBJS list) will form the library.
set(LIB_SRCS
  adj_fnh.f
  cgm_util.f
  environ.f
  f2ursi.f
  gen.F
  geophys_util.F
  hlim.f
  iau_time.f
  init.F
  io_util.f
  jma_igrf10.f
  looklib.F
  low_para.f
  math_uti.F
  mid_para.f
  param.F
  pim_asub.f
  pim_asub_c.c
  pim_blok.f
  precip.f
  read_dba.f
  read_geophys.F
  read_vlbi.f
  rtd.f
  setupvlbi.f
  strnglib.f
  teccalc.F
  tecvlbi4.f
  tijdsub.f
  timelib.f
  usumodel.f
)

# Test programs (not part of the library) – adjust filenames as needed.
set(TESTMAIN1 testmain.f)   # for pim_asub
set(TESTMAIN2 testmain2.f)  # for pim_asub2

# --------------------------------------------------------------------
# External Libraries
#
# Define the external SOFA library path (adjust this relative path as needed).
# Also, if your Fortran compiler uses a runtime library (F77_COMPILER_LIB),
# you can specify its name here.
# --------------------------------------------------------------------
set(SOFA_LIB "${CMAKE_CURRENT_SOURCE_DIR}/../../../../../../FORTRAN/SOFA/sofa/libsofa.so")
if(NOT DEFINED F77_COMPILER_LIB)
  set(F77_COMPILER_LIB "")
endif()

# --------------------------------------------------------------------
# Build library targets from LIB_SRCS
#
# Create a shared library (which will produce libpim.so) and a static library
# (which will produce libpim.a). We use the output name "pim" so that on UNIX
# the actual files become libpim.so and libpim.a.
# --------------------------------------------------------------------
add_library(pim SHARED ${LIB_SRCS})
set_target_properties(pim PROPERTIES OUTPUT_NAME "pim")

add_library(pim_static STATIC ${LIB_SRCS})
set_target_properties(pim PROPERTIES OUTPUT_NAME "pim" 
 VERSION "${VER_MAJOR}.${VER_MINOR}.${VER_SUBSUB}"
  SOVERSION "${VER_MAJOR}")

# --------------------------------------------------------------------
# Build Executable Targets
#
# Each executable is built by compiling a test program and then linking
# in the common library (pim_shared) plus the external libraries.
# --------------------------------------------------------------------
add_executable(pim_asub ${TESTMAIN1})
target_link_libraries(pim_asub PRIVATE pim ${SOFA_LIB} ${F77_COMPILER_LIB} m)

#add_executable(pim_asub2 ${TESTMAIN2})
#target_link_libraries(pim_asub2 PRIVATE pim ${SOFA_LIB} ${F77_COMPILER_LIB} m)

# --------------------------------------------------------------------
# Installation
#
# Install both the libraries (shared and static) and the header(s).
# The libraries will be placed in the "lib" directory and the headers
# in the "include" directory of the install prefix.
# --------------------------------------------------------------------
install(TARGETS pim pim_static
        LIBRARY DESTINATION ${INSTALLDIR}/lib 
        ARCHIVE DESTINATION ${INSTALLDIR}/lib
        RUNTIME DESTINATION ${INSTALLDIR}/bin)
install(FILES ${HEADERS} DESTINATION ${INSTALLDIR}/include)

# --------------------------------------------------------------------
# Custom Targets for Dependency Generation and Cleaning
#
# CMake generators already provide a "clean" target. You can add custom
# targets for dependency generation if needed. The following is a placeholder.
# --------------------------------------------------------------------
add_custom_target(vlb4_depend
  COMMAND ${CMAKE_COMMAND} -E echo "Dependency generation not implemented."
  COMMENT "Generating dependencies"
)

# --------------------------------------------------------------------
# Notes:
#
# - The original Makefile built executables by directly linking object files.
#   In CMake, we instead build a library from the common sources and link that library.
#
# - If your test program source files have extensions other than ".f" (or if
#   they need special compile options), you can set source file properties or add
#   separate targets accordingly.
#
# - The linking of external libraries (e.g. SOFA and the Fortran runtime) is
#   done via target_link_libraries. Adjust these as needed.
#
# - CMake’s built-in "clean" target (e.g. using "cmake --build . --target clean")
#   will remove build artifacts. The "distclean" functionality (removing backup
#   files) can be added as an additional custom target if desired.
# --------------------------------------------------------------------
