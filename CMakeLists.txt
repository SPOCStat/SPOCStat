cmake_minimum_required(VERSION 3.10)

cmake_policy(VERSION 3.10.3...3.13.1)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/cmake)

include(CMakeHeader)

project(SPOCS
  VERSION 1.0
  HOMEPAGE_URL "https://github.com/SPOCStat/SPOCStat#readme"
  DESCRIPTION "SPOCS: Single-point, Parallel, Online, Converging Statistics library"
  LANGUAGES C Fortran)

include(checkOutOfSource)
include(FortranBiggestInt)
Fortran_biggest_int()
include(CompilerSpecificSettings)



set(cache_size_lib_srcs CacheLineSize.f90 CacheLineSize.c CacheLineSize.h)
foreach(src IN LISTS cache_size_lib_srcs)
  list(APPEND cache_info_srcs ${CMAKE_SOURCE_DIR}/src/introspection/${src})
endforeach()

add_library(cache_info ${cache_info_srcs})
set_property(TARGET cache_info
  PROPERTY
  LINKER_LANGUAGE Fortran)

add_executable(test_cache_info ${CMAKE_SOURCE_DIR}/src/introspection/tests/testCacheLineSize.f90)
target_link_libraries(test_cache_info cache_info)

enable_testing()

add_test(NAME test_cache_info
  COMMAND ./test_cache_info)
set_property(TEST test_cache_info
  PROPERTY
  PASS_REGULAR_EXPRESSION "Test Passed!")

# -mstrict-align for gcc
