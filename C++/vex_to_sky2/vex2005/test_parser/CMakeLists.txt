cmake_minimum_required(VERSION 3.10)
project(MyProject C)

# Assume main.c is the source file (which, when compiled, produces main.o)
add_executable(v main.c)

# Link with the static library located at ../vex.a and with the -lfl library.
# Here, ${CMAKE_CURRENT_SOURCE_DIR} returns the directory of the CMakeLists.txt file.
target_link_libraries(v PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../vex
    fl
)

