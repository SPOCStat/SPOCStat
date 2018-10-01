cmake_minimum_required (VERSION 2.8.5 FATAL_ERROR)
include ( ../../checkOutOfSource.cmake )
include ( ../../CMakeHeader.cmake )
enable_language(C)
enable_language(Fortran)
include ( ../../compiler-specific-settings.cmake )
include ( ../../Fortran_biggest_int.cmake )
if (PRINT_CALL_STACK)
  add_definitions(-DMY_BACKTRACE)
  add_definitions(-DDEBUGGING)
endif(PRINT_CALL_STACK)

project (common_modules NONE)

Fortran_biggest_int()

set (F2008_FILES kinds.F90 stringhelper.F90)
set (UNUSED_DUMMY_ARGS )
set (LONG_INCLUDE_LINES )
set (NEEDS_REALLOC_RHS stringhelper.F90)
#Encapsulate in a function/macro/module?
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
endif( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel" )

add_library (common STATIC stringhelper.F90 kinds.F90 physical_const.F90 assertion.F90)

set (TestEXE "CommonFortranTests")
# Give testing stuff a whirl
create_test_sourcelist (CommonTests ${TestEXE}.c
			testtemplate.F90
			teststringhelper.F90)
add_executable (${TestEXE} ${CommonTests})
set_target_properties (${TestEXE} PROPERTIES LINKER_LANGUAGE "C")
target_link_libraries (${TestEXE} common)

# Remove driver source file
set (TestsToRun ${CommonTests})
list (REMOVE_ITEM TestsToRun ${TestEXE}.c)

enable_testing()

# Set all tests
foreach (test ${TestsToRun})
  get_filename_component (TName ${test} NAME_WE)
  add_test (NAME ${TName} #CONFIGURATIONS Release
    COMMAND ${TestEXE} ${TName} ${TArgs})
  #add_test (NAME ${TName}_debug CONFIGURATIONS Debug
  #  COMMAND valgrind --leak-check=yes $<TARGET_FILE:${TestEXE}> ${TName} ${TArgs})
endforeach ()
