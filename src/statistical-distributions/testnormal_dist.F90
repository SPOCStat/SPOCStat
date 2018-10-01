function testnormal_dist(argc,argv) result(res) bind(c,name='testnormal_dist')
  use iso_c_binding ,only: c_int ,c_ptr
  use normal_dist_m ,only: BoxMuller ,MarsagliaPolar
  use kinds_m       ,only: WP ,WI
  implicit none
# ifdef NLOOPS
  integer(WI) ,parameter            :: LOOPS = NLOOPS
# else
  integer(WI) ,parameter            :: LOOPS = 1
# endif
  integer(WI) ,parameter            :: NSAMPLES = 500000
  real(WP)    ,parameter            :: TOL = 0.09_WP ,PRESCRIBED_MEAN = 5.0_WP !This will improve condition # of sums
  integer(c_int) ,intent(in) ,value :: argc
  type(c_ptr)    ,intent(in)        :: argv
  integer(c_int)                    :: res
  real(WP)                          :: mean ,var ,stdev ,min ,max ,t1 ,t2 ,skewness ,kurtosis ,&
                                       BMtime ,MPtime
  real(WP) ,allocatable             :: BMVars(:) ,MPVars(:)
  integer                           :: n

  ! Since tests are linked with a c compiler deal with different Fortran compilers idiosyncrasies
  ! for initializing the Fortran run-time library (FRTL)
#include "../common/fort_init_RTL.F90"

  res = 0
  print*, 'Tolerance = ', TOL
  allocate(BMVars(NSAMPLES),MPVars(NSAMPLES))
  call CPU_TIME(t1)
  do n = 1,LOOPS
     call BoxMuller(BMVars)
  end do
  call CPU_TIME(t2)
  BMtime = t2 - t1
  print*, LOOPS, ' calls to BoxMuller took ', BMtime, ' seconds with array size:' ,NSAMPLES
  BMVars = BMVars + PRESCRIBED_MEAN
  call compute_stats(BMVars)
  if ( .not. correct_stats() ) res = 1

  call CPU_TIME(t1)
  do n = 1,LOOPS
     call MarsagliaPolar(MPVars)
  end do
  call CPU_TIME(t2)
  MPtime = t2 - t1
  print*, LOOPS, ' calls to MarsagliaPolar took ', MPtime, ' seconds with array size:' ,NSAMPLES
  MPVars = MPVars + PRESCRIBED_MEAN
  call compute_stats(MPVars)
  if ( .not. correct_stats() ) res = 1
  if (MPtime < BMtime) then
     print*, 'MarsagliaPolar algorithm is ', BMtime/MPtime, ' times faster than BoxMuller&
          &(for this configuration).'
  else
     print*, 'BoxMuller algorithm is ', MPtime/BMtime, ' times faster than MarsagliaPolar&
          &(for this configuration).'
  end if
  deallocate(MPVars,BMVars)
contains
  function E(vec) result(res)
    !Expected value using numerically stable on-line algorithm with Kahan summation
    !With ifort, Kahan summation requires -assume protect_parens
    real(WP) ,intent(in) :: vec(:)
    real(WP)             :: res ,tmp ,err ,y
    integer(WI)          :: i
    res = 0.0_WP
    tmp = 0.0_WP
    err = 0.0_WP
    y   = 0.0_WP
    do i = 1,size(vec)
       y   = (vec(i) - res)/i + err !Update for expected value, plus correction from last time
       tmp = res + y
       err = (tmp - res) - y        !residual/error correction
       res = tmp
    end do
  end function
  subroutine compute_stats(vec)
    real(WP), intent(in)   :: vec(:)
    integer(WI) ,parameter :: nos = NSAMPLES
    min      = minval(vec)
    max      = maxval(vec)
    mean     = E(vec)
    var      = E((vec - mean)**2)
    stdev    = sqrt(var)
    skewness = E((vec - mean)**3)/(stdev**3)
    kurtosis = E((vec - mean)**4)/(var**2) - 3.0_WP
    print*, 'minimum  = ', min
    print*, 'maximum  = ', max
    print*, 'range    = ', max - min
    print*, 'mean     = ', mean
    print*, 'variance = ', var
    print*, 'stdev    = ', stdev
    print*, 'skewness = ', skewness
    print*, 'kurtosis = ', kurtosis
  end subroutine compute_stats
  function correct_stats() result(res)
    logical :: res
    res = .true.
    res = res .and. (abs(mean - PRESCRIBED_MEAN) < TOL) !mean     = 0
    res = res .and. (abs(stdev - 1.0_WP)         < TOL) !stdev    = 1
    res = res .and. (abs(skewness)               < TOL) !skewness = 0
    res = res .and. (abs(kurtosis)               < TOL) !kurtosis = 0 (excess)
  end function
end function
