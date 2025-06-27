cmake_minimum_required(VERSION 3.10)
project(JMA_code)


# Install all files (except CMakeLists.txt) from the current directory into
# libdata/PIM/cgmdb. Adjust the FILES_MATCHING pattern as needed.
set(DATA cglalo.dat cglalo.dat7)

foreach(file ${DATA})
  install(FILES ${file} DESTINATION ${INSTALLDIR}/libdata/PIM/cgmdb
)
endforeach()

# Add a post-install command that "touches" an empty file named install.
install(CODE "file(WRITE \"${INSTALLDIR}/libdata/PIM/cgmdb/install\" \"\")"
  COMMENT "Touching install file"
)

