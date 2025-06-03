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

add_executable(iritestne iritestne.f ${OBJS} ${NEOBJS})
target_link_libraries(iritestne ${LIBRARIES})

# Build shared library

add_library(iri SHARED ${OBJS} ${NOBJS})
set_target_properties(iri PROPERTIES VERSION ${VER_MAJOR}.${VER_MINOR}.${VER_SUBSUB})


# make_ap executable

add_executable(make_ap make_ap.cxx)
target_link_libraries(make_ap ${LIBRARIES})

# Installation rules
install(TARGETS iri LIBRARY DESTINATION ${INSTALLDIR}/lib )
install(FILES ${HEADERS} DESTINATION ${INSTALLDIR}/include)






