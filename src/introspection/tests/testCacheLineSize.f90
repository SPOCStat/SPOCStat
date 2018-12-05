program testCacheLineSize
  use :: CacheInfo, only: cache_line_size, get_page_size
  use, intrinsic :: ISO_FORTRAN_ENV, only: stdout => output_unit, stderr => error_unit, int64
  implicit none
  character(len=*), parameter :: CSV_FLAG="--csv" !! Comma separated value output
  integer :: cmd_len=-1, ierr=-2
  integer(int64) :: page_size, line_size, lines_per_page
  character(len=:), allocatable :: full_cmd_line

  ! Check for option flags in command line
  ! First retrieve
  call get_command(length=cmd_len)
  if(cmd_len < 1) error stop "Unable to get command line length."
  allocate(character(len=cmd_len) :: full_cmd_line)
  call get_command(command=full_cmd_line, status=ierr)
  if(ierr /= 0) error stop "Unable to retrieve command line."

  ! Fetch the values of interest
  page_size = get_page_size()
  line_size = cache_line_size()
  if (line_size == 0) error stop "Could not determine cache line size!"
  lines_per_page = page_size / line_size

  ! Determine how to print the data and print it
  if( index(full_cmd_line, CSV_FLAG) == 0) then
     ! Flag NOT found
     write(stdout,'(A,I0,A)') "Page size is ", page_size, " bytes."
     write(stdout,'(A,I0,A)') "Cache line size is ", line_size, " bytes."
     write(stdout,'(A,I0,A)') "There are ", lines_per_page, " cache lines per page."
     write(stdout,'(A)') "Test Passed!"
  else
     ! CSV flag found
     write(stdout,'(A)') "page size (bytes), line size (bytes), lines-per-page"
     write(stdout,'(I0,2(", ",I0))') page_size, line_size, lines_per_page
  end if
end program
