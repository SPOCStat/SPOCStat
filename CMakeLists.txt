cmake_minimum_required(VERSION 3.10)

project(SPOCS C Fortran)

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
