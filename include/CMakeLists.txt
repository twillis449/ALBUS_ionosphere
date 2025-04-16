cmake_minimum_required(VERSION 3.10)
project(JMA_code_install)

# Users can override CMAKE_INSTALL_PREFIX at configuration or install time.


install(FILES 
  JMA_code.h
  JMA_math.h
  f2c.h
  DESTINATION ${INSTALLDIR}/include
)



