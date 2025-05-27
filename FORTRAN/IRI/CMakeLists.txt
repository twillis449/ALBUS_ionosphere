cmake_minimum_required(VERSION 3.10)
project(JMA_IRI LANGUAGES C CXX Fortran)

set(CMAKE_Fortran_FLAGS "-O2")
set(CMAKE_C_FLAGS "-O2")
set(CMAKE_CXX_FLAGS "-O2")

set(HEADERS
    jma_irisub_c.h
)

set(SRCS
    cira.f
    igrf.f
    iridreg.f
    irifun.f
   
)

set(OBJS
    cira.f
    igrf.f
    iridreg.f
    irifun.f
)

set(NOBJS
    irisub.f
    iritec.f
    jma_irisub.F
    jma_irisub_c.c
)

set(NEOBJS
    irisubne.f
    iritecne.f
)

# Build executable targets
#add_executable(iritest iritest.f ${OBJS} ${NOBJS})
#target_link_libraries(iritest ${LIBRARIES})

add_executable(iritestne iritestne.f ${OBJS} ${NEOBJS})
target_link_libraries(iritestne ${LIBRARIES})

# Build shared library

add_library(iri SHARED ${OBJS} ${NOBJS})
set_target_properties(iri PROPERTIES VERSION ${VER_MAJOR}.${VER_MINOR}.${VER_SUBSUB})



# Build static library
add_library(iri_static STATIC ${OBJS} ${NOBJS})

# make_ap executable


add_executable(make_ap make_ap.cxx)
target_link_libraries(make_ap ${LIBRARIES})

# Installation rules
install(TARGETS iri iri_static LIBRARY DESTINATION ${INSTALLDIR}/lib ARCHIVE DESTINATION ${INSTALLDIR}/lib)
install(FILES ${HEADERS} DESTINATION ${INSTALLDIR}/include)
install(DIRECTORY DESTINATION ${INSTALLDIR}/libdata/IRI)

# Cleanup rules
#if(NOT TARGET clean)
#   add_custom_target(clean COMMAND ${CMAKE_COMMAND} -E remove ${OBJS} ${NOBJS} ${NEOBJS} make_ap.o iritest.o iritestne.o libiri.so*)
#endif()

#if(NOT TARGET distclean)
#    add_custom_target(distclean COMMAND ${CMAKE_COMMAND} -E remove ${OBJS} ${PROG})
#endif()