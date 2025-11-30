cmake_minimum_required(VERSION 3.23)

project(AoC2025)

set(CMAKE_CXX_STANDARD "20")
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

macro(add_day day)
  add_executable(Day${day} src/day${day}.cpp)
endmacro()

foreach(day RANGE 1 25)
  if(day LESS 10)
    set(day "0${day}")
  endif()
  if(EXISTS "${CMAKE_SOURCE_DIR}/src/day${day}.cpp")
    add_day(${day})
  endif()
endforeach()
