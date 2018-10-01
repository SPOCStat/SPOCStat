cmake_minimum_required (VERSION 2.8.5 FATAL_ERROR)
include ( ../../checkOutOfSource.cmake )
include ( ../../CMakeHeader.cmake )
enable_language(C)
enable_language(Fortran)
include ( ../../compiler-specific-settings.cmake )
include ( ../../Fortran_biggest_int.cmake )
project (SinglePointStreamingStats NONE)

if (PRINT_CALL_STACK)
  add_definitions(-DMY_BACKTRACE)
  add_definitions(-DDEBUGGING)
endif(PRINT_CALL_STACK)

Fortran_biggest_int()

set (F2008_FILES ../common/kinds.F90 spocs_accuracy.F90
                 ../statistical-distributions/normal_dist.F90)
set (UNUSED_DUMMY_ARGS covN_factory.F90 testbinomial.F90 testspocs.F90)
set (LONG_INCLUDE_LINES testbinomial.F90 spocs_cov_factory.F90 testspocs.F90 covN.F90 statN.F90
                        ../common/assertion.F90)
set (NEEDS_REALLOC_RHS testspocs.F90 spocs_accuracy.F90)
set (INTENTIONAL_REAL_COMP ../common/assertion.F90 testspocs.F90)

if ( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel" )
  set_property ( SOURCE ${F2008_FILES}
    APPEND_STRING
    PROPERTY COMPILE_FLAGS " -stand f08 -diag-disable 10121")
  set_property ( SOURCE ${UNUSED_DUMMY_ARGS}
    APPEND_STRING
    PROPERTY COMPILE_FLAGS " -warn nounused")
  set_property ( SOURCE ${LONG_INCLUDE_LINES}
    APPEND_STRING
    PROPERTY COMPILE_FLAGS " -diag-disable 5268 -diag-disable 5194")
  set_property ( SOURCE ${NEEDS_REALLOC_RHS}
    APPEND_STRING
    PROPERTY COMPILE_FLAGS " -assume realloc_lhs")
  set_property ( SOURCE ../common/kinds.F90
    APPEND_STRING
    PROPERTY COMPILE_FLAGS " -diag-disable 8430")
  # Nothing to be done to disable real (in)equality comparisons for Intel
elseif ( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU" )
  set_property ( SOURCE ${F2008_FILES}
    APPEND_STRING
    PROPERTY COMPILE_FLAGS " -std=f2008")
  set_property ( SOURCE ${UNUSED_DUMMY_ARGS}
    APPEND_STRING
    PROPERTY COMPILE_FLAGS " -Wno-unused-dummy-argument")
  set_property ( SOURCE ${NEEDS_REALLOC_RHS}
    APPEND_STRING
    PROPERTY COMPILE_FLAGS " -frealloc-lhs")
  set_property ( SOURCE ${LONG_INCLUDE_LINES}
    APPEND_STRING
    PROPERTY COMPILE_FLAGS " -ffree-line-length-none")
  set_property ( SOURCE ${INTENTIONAL_REAL_COMP}
    APPEND_STRING
    PROPERTY COMPILE_FLAGS " -Wno-compare-reals")
endif( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel" )

set(USE_SINGLE_PRECISION "FALSE"
  CACHE BOOL "Choose single precision for the working precision, defaults to double precision")

if(USE_SINGLE_PRECISION)
  add_definitions(-DMY_WP=SP)
else()
  add_definitions(-DMY_WP=DP)
endif()

add_library (spocs ../common/kinds.F90
                   binomial.F90
		   spocs.F90
		   spocs_cov.F90
		   covN.F90
		   statN.F90
		   spocs_cov_factory.F90
		   covN_factory.F90)

add_executable (spocs_accuracy spocs_accuracy.F90 ../common/physical_const.F90)
target_link_libraries ( spocs_accuracy spocs)

set (TestEXE "SPSSFortranTests")
create_test_sourcelist (SPSSTests ${TestEXE}.c
			testbinomial.F90
			testspocs.F90)
add_executable (${TestEXE} ${SPSSTests} ../statistical-distributions/normal_dist.F90
                           ../common/assertion.F90)
set_target_properties (${TestEXE} PROPERTIES LINKER_LANGUAGE "C")
target_link_libraries (${TestEXE} spocs)

set (TestsToRun ${SPSSTests})
list (REMOVE_ITEM TestsToRun ${TestEXE}.c)

enable_testing()
foreach (test ${TestsToRun})
  get_filename_component(TName ${test} NAME_WE)
  add_test (NAME ${TName} #CONFIGURATIONS Release
    COMMAND ${TestEXE} ${TName} ${TArgs})
endforeach ()
