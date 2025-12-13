cmake_minimum_required(VERSION 3.23)

project(AoC2025)

set(CMAKE_CXX_STANDARD "26")
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_FLAGS "-O2")

find_package(PkgConfig REQUIRED)
pkg_check_modules(Z3 REQUIRED z3)
add_library(Z3::z3 INTERFACE IMPORTED)

target_include_directories(Z3::z3 INTERFACE ${Z3_INCLUDE_DIRS})
target_link_directories(Z3::z3 INTERFACE ${Z3_LIBRARY_DIRS})
target_link_libraries(Z3::z3 INTERFACE ${Z3_LIBRARIES})

add_library(utils)
target_sources(
  utils
  PRIVATE src/utils/cli.cpp src/utils/file.cpp
  PUBLIC FILE_SET
         HEADERS
         BASE_DIRS
         src
         FILES
         src/utils/cli.h
         src/utils/file.h)

macro(add_day day)
  add_executable(Day${day} src/day${day}.cpp)
  target_link_libraries(Day${day} PRIVATE utils Z3::z3)
endmacro()

foreach(day RANGE 1 25)
  if(day LESS 10)
    set(day "0${day}")
  endif()
  if(EXISTS "${CMAKE_SOURCE_DIR}/src/day${day}.cpp")
    add_day(${day})
  endif()
endforeach()
