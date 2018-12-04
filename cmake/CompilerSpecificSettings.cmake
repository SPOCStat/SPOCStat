# Fortran language should already be enabled via enable language
if ( NOT CompilerSpecificSettingsInit ) #then we haven't run this code yet
  set (CompilerSpecificSettingsInit TRUE)

  if ( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel" )
    set (MY_Fortran_FLAGS "-warn -traceback -stand f08" CACHE STRING
      "Set additional Intel compiler Fortran compilation flags")
    set (ENABLE_RUNTIME_CHECKS OFF CACHE BOOL
      "Enable all Intel Fortran compiler runtime checks?")
    if (ENABLE_RUNTIME_CHECKS)
      set (MY_Fortran_FLAGS "${MY_Fortran_FLAGS} -check")
    endif()
    set (STATIC_ANALYSIS OFF CACHE BOOL
      "Turn on static analysis? (Note: You need Intel Inspector XE to view analysis. Forces IPO too.)")
    set (STATIC_ANALYSIS_LEVEL sc2 CACHE STRING
      "Choose the level of static analysis: sc3 (critical, error and warning), sc2 (critical and error) or sc1 (critical only)")
    set_property (CACHE STATIC_ANALYSIS_LEVEL PROPERTY STRINGS sc1 sc2 sc3)
    set (STATIC_ANALYSIS_MODE "full" CACHE STRING
      "Set the static analysis mode: full (most false positives, fewer false negatives)
concise (reduce false positives more than false negatives) and precise (elminates as many false positives as possible)")
    set_property (CACHE STATIC_ANALYSIS_MODE PROPERTY STRINGS full concise precise)
    if (STATIC_ANALYSIS)
      set (MY_Fortran_FLAGS "${MY_Fortran_FLAGS} -diag-enable ${STATIC_ANALYSIS_LEVEL} -diag-enable sc-${STATIC_ANALYSIS_MODE}")
    endif()
  elseif ( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU" )
    set (MY_Fortran_FLAGS "-fbacktrace -Wall -ffree-line-length-none -Wextra -pedantic -std=f2008 -fno-realloc-lhs -Wno-unused-parameter -Wno-maybe-uninitialized" CACHE STRING
      "Set additional GNU compiler Fortran compilation flags")
    set (ENABLE_RUNTIME_CHECKS OFF CACHE BOOL
      "Enable all GNU Fortran compiler runtime checks?")
    if (ENABLE_RUNTIME_CHECKS)
      set (MY_Fortran_FLAGS "${MY_Fortran_FLAGS} -fcheck=all")
    endif()
  else()
    message("\n*** Untested Fortran compiler detected: ${CMAKE_Fortran_COMPILER_ID}.***\n")
    message("\n*** Attempting to build anyway. Please report any failures to the compiler vendor.***\n")
  endif()
  if ( NOT MY_Fortran_FLAGS_ADDED )
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${MY_Fortran_FLAGS}")
    message (STATUS
      "Building configuration: ${CMAKE_BUILD_TYPE}")
    message (STATUS
      "Adding flags for ${CMAKE_Fortran_COMPILER_ID} Fortran compiler:  " "${MY_Fortran_FLAGS}")
    set (MY_Fortran_FLAGS_ADDED "TRUE")
  endif()
  set_directory_properties (PROPERTIES
    INTERPROCEDURAL_OPTIMIZATION_Release ON INTERPROCEDURAL_OPTIMIZATION_RelWithDebInfo ON)
  if (${STATIC_ANALYSIS}) #we need to link & archive with XILD and XIAR
    set_directory_properties (PROPERTIES INTERPROCEDURAL_OPTIMIZATION ON)
  endif()
endif()
