cmake_minimum_required (VERSION 2.8.5 FATAL_ERROR)
include ( ../../checkOutOfSource.cmake )
include ( ../../CMakeHeader.cmake )
enable_language(C)
enable_language(Fortran)
include ( ../../compiler-specific-settings.cmake )

project (statistical_distributions)

if (PRINT_CALL_STACK)
  add_definitions(-DMY_BACKTRACE)
endif(PRINT_CALL_STACK)

set(TEST_ITERATIONS 1
  CACHE STRING "How many times should the normal distribution
test run the BM and MP algorithms for timing purposes?
(MUST be an ineger)")

add_library (statdist STATIC ../common/kinds.F90 normal_dist.F90)

if ( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel" )
  set_source_files_properties(testnormal_dist.F90 PROPERTIES COMPILE_FLAGS "-assume protect_parens -fp-model precise")
endif( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel" )

set (TestEXE "DistributionsFortranTests")
# Give testing stuff a whirl
create_test_sourcelist (CommonTests
                        ${TestEXE}.c
			testnormal_dist.F90)
if ( "${TEST_ITERATIONS}" MATCHES "^[0-9]+$" )
  set_source_files_properties(testnormal_dist.F90 PROPERTIES COMPILE_FLAGS -DNLOOPS=${TEST_ITERATIONS})
else( "${TEST_ITERATIONS}" MATCHES "^[0-9]+$" )
  message( SEND_ERROR
    "TEST_ITERATIONS must be set to a positive integer, not: ${TEST_ITERATIONS}")
endif( "${TEST_ITERATIONS}" MATCHES "^[0-9]+$" )

add_executable (${TestEXE} ${CommonTests})
set_target_properties (${TestEXE} PROPERTIES LINKER_LANGUAGE "C")
target_link_libraries (${TestEXE} statdist)

# Remove driver source file
set (TestsToRun ${CommonTests})
list (REMOVE_ITEM TestsToRun ${TestEXE}.c)

enable_testing()

# Set all tests
foreach (test ${TestsToRun})
  get_filename_component (TName ${test} NAME_WE)
  add_test (NAME ${TName} #CONFIGURATIONS Release
    COMMAND ${TestEXE} ${TName} ${TArgs})
endforeach ()
