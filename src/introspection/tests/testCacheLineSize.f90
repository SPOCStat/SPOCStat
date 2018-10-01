program testCacheLineSize
  use :: CacheLineSize, only: cache_line_size, getpagesize
  use, intrinsic :: ISO_FORTRAN_ENV, only: stdout => output_unit, stderr => error_unit
  implicit none

  write(stdout,'(A,I0,A)') "Page size is ", getpagesize(), " bytes."
  if ( cache_line_size() == 0) then
     write(stderr,'(A)') "Test Failed!"
     write(stderr,'(A)') "Could not determine cache line size!"
     error stop 500
  else
     write(stdout,'(A,I0,A)') "Cache line size is ", cache_line_size(), " bytes."
     write(stdout,'(A,I0,A)') "There are ", getpagesize()/cache_line_size(), " cache lines per page."
     write(stdout,'(A)') "Test Passed!"
     stop 0
  end if
end program
