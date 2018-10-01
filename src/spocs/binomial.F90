module binomial_m
  use kinds_m ,only: WI
  implicit none
  private
  public :: binomial_coeff
# ifndef MY_BACKTRACE
# define IS_PURE pure
# else
# define IS_PURE
# endif
contains
  IS_PURE function binomial_coeff(n) result(res) !returns vector of binomial
                                                 !coefficients for (1 + x)^n
    !Client code should store results as needed to avoid calling this routine multiple
    !times for the same n
    integer(WI) ,intent(in)     :: n
    integer(WI) ,dimension(0:n) :: res
    integer(WI) ,pointer        :: curr(:) ,prv(:) ,tmp(:)
    integer                     :: i ,j
#   ifdef MY_BACKTRACE
    print*, 'Entering binomial_coeff in ', __FILE__
#   endif
    allocate(curr(0:n),prv(0:n))
    tmp=>null()
    do i=0,n !construct via Pascal's triangle
       tmp  => prv
       prv  => curr
       curr => tmp  !exchange
       do j=0,i
          if ( j == 0 .or. j == i ) then
             curr(j) = 1_WI
          else
             curr(j) = prv(j) + prv(j-1)
          end if
       end do
    end do
    res = curr
    deallocate(curr,prv)
    curr=>null() ;prv=>null() ;tmp=>null()
#   ifdef MY_BACKTRACE
    print*, 'Leaving binomial_coeff in ', __FILE__
#   endif
  end function
end module
!!$program test_binomial
!!$  use binomial_m ,only: binomial_coeff
!!$  implicit none
!!$  integer ,parameter :: n = 6
!!$  integer            :: i
!!$
!!$  print*, 'Printing the first ' ,n+1 ,' rows of Pascal''s trianlge:'
!!$  do i = 0,n
!!$     print*, binomial_coeff(i)
!!$  end do
!!$end program
