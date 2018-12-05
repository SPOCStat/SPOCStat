# Macro to find largest available integer and make available via CMake variable and define.
macro(Fortran_biggest_int)
  message(STATUS "Checking for the largest available Fortran integer..")
  set(filename ${CMAKE_BINARY_DIR}/getbigint.f90)
  file(WRITE  ${filename} "program getbigint
  implicit none
  integer ,parameter :: DEFAULT = kind(1) , DEF_RANGE = range(kind(1))
  integer :: r, ktmp, k
  r = DEF_RANGE
  k = DEFAULT
  do
     r    = r + 1
     ktmp = selected_int_kind(r)
     if ( ktmp == -1 ) exit
     k = ktmp
  end do
  write(*,'(i0, a ,i0)') k ,'; ' ,range(k)
end program\n")
  try_run(BIG_INT_RUNS BIG_INT_COMPILES
    ${CMAKE_BINARY_DIR} ${filename}
    RUN_OUTPUT_VARIABLE FBIGINT)
  string(REGEX REPLACE "\n" "" FBIGINT "${FBIGINT}")
  message(STATUS "Fortran big integer kind; range: ${FBIGINT}")
  list(REMOVE_AT FBIGINT 1) #lists start at 0
  add_definitions(-DBIGINT=${FBIGINT})
endmacro()
