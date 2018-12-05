#-=============================
# General project configuration
#-=============================

# Indicate which versions of CMake we support
# & default behaviors we want
cmake_minimum_required(VERSION 3.10)

# Policies provide warnings to developers and
# ensure backwards compatibility. Indicate versions
# that we know work to cmake_policy()
cmake_policy(VERSION 3.10.3...3.13.1)

# Organize things in visual studio
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# Tell CMake to look in ./cmake for include modules
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/cmake)

# Set the type/configuration of build to perform, if not set by the user
include(CMakeHeader)

# Print a fancy banner
set(PROJ SPOCS) # Define project name early
file(STRINGS ${CMAKE_CURRENT_LIST_DIR}/.VERSION PROJ_VER
  LIMIT_COUNT 1)
file(READ ${CMAKE_CURRENT_LIST_DIR}/assets/${PROJ}_banner.txt ${PROJ}_banner)
string( REGEX REPLACE "____/\n" "____/" ${PROJ}_banner "${${PROJ}_banner}")
# Informational messages
message( STATUS
  "
===============================================================
${${PROJ}_banner}   Version ${PROJ_VER}

---------------------------------------------------------------
   Building ${PROJ}, configuration: ${CMAKE_BUILD_TYPE}
===============================================================

")
message( STATUS "Running with CMake from: ${CMAKE_COMMAND}" )
message( STATUS "Current source dir for Project: ${CMAKE_CURRENT_LIST_DIR}")
message( STATUS "Current build directory for Project: ${CMAKE_BINARY_DIR}")
string(TIMESTAMP TODAY UTC)
message( STATUS "Built by $ENV{USER} at ${TODAY} (UTC)")

# Declare project name, description, version, languages and homepage
project(${PROJ}
  VERSION ${PROJ_VER}
  HOMEPAGE_URL "https://github.com/SPOCStat/SPOCStat#readme"
  DESCRIPTION "SPOCS: Single-point, Parallel, Online, Converging Statistics library"
  LANGUAGES C Fortran)

include(checkOutOfSource)
include(CompilerSpecificSettings)
include(FortranBiggestInt)
Fortran_biggest_int()


set(cache_size_lib_srcs CacheLineSize.f90 CacheLineSize.c GetPageSize.c)
foreach(src IN LISTS cache_size_lib_srcs)
  list(APPEND cache_info_srcs ${CMAKE_CURRENT_LIST_DIR}/src/introspection/${src})
endforeach()

# WARNING! AFAICT, try_compile() processes sources *in the order listed* and does not seem to attempt
# any sort of Fortran compilation order dependency resolution
try_compile(CACHE_INFO_BUILT ${CMAKE_BINARY_DIR}
  SOURCES ${cache_info_srcs} ${CMAKE_CURRENT_LIST_DIR}/src/introspection/tests/testCacheLineSize.f90
  CMAKE_FLAGS -DCMAKE_Fortran_LINKER_PREFERENCE:BOOL=1
    -DCMAKE_INCLUDE_DIRECTORIES:PATH=${CMAKE_CURRENT_LIST_DIR}/src/introspection
  OUTPUT_VARIABLE CACHE_INFO_TRY_COMP_OUTPUT
  COPY_FILE ${CMAKE_BINARY_DIR}/get_cache_info
  )

if("${CACHE_INFO_BUILT}")
  execute_process(COMMAND ${CMAKE_BINARY_DIR}/get_cache_info --csv
    OUTPUT_VARIABLE mem_info_list
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  string(REGEX REPLACE "^.*[\n]" "" mem_info_list "${mem_info_list}")
  string(REPLACE "," ";" mem_info_list "${mem_info_list}")
  list(GET mem_info_list 0 page_size)
  list(GET mem_info_list 1 line_size)
  list(GET mem_info_list 2 lines_per_page)
  message(STATUS "page size, line size, lines-per-page = ${page_size}, ${line_size}, ${lines_per_page}")
else()
  message(WARNING "Cache info introspection for current host failed with message:
  ${CACHE_INFO_TRY_COMP_OUTPUT}")
endif()

add_library(cache_info ${cache_info_srcs})
set_property(TARGET cache_info
  PROPERTY
  LINKER_LANGUAGE Fortran)

add_executable(test_cache_info ${CMAKE_CURRENT_LIST_DIR}/src/introspection/tests/testCacheLineSize.f90)
target_link_libraries(test_cache_info cache_info)

enable_testing()

add_test(NAME test_cache_info
  COMMAND ./test_cache_info)
set_property(TEST test_cache_info
  PROPERTY
  PASS_REGULAR_EXPRESSION "Test Passed!")

# -mstrict-align for gcc
