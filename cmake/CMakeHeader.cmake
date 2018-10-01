# Preempt the project command in setting the following cache variables
set (CMAKE_CONFIGURATION_TYPES "Release" "RelWithDebInfo" "Debug")
set(CMAKE_BUILD_TYPE "Release" CACHE STRING
  "Choose the type of build, options are: None(CMAKE_Fortran_FLAGS or CMAKE_C_FLAGS used) ${CMAKE_CONFIGURATION_TYPES}.")
set_property (CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS ${CMAKE_CONFIGURATION_TYPES})
set (PRINT_CALL_STACK OFF CACHE BOOL
  "Turn on print statements upon entering and leaving non-elemental procedures, where provided.")
