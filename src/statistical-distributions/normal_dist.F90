module normal_dist_m
  use kinds_m ,only: WP ,WI
  implicit none
  private
  public :: BoxMuller ,MarsagliaPolar ,RanVec
  abstract interface
     subroutine RanVec(harvest) !uniform dist on (0,1)
       import :: WP
       real(WP) ,intent(out) :: harvest(:)
     end subroutine
  end interface
contains !Need a pure uniform RNG, if these are to be pure/stateless/||
         !This would require user supplied uniform RNG which explicitly
         !passes the state as an argument.
  subroutine BoxMuller(harvest,ran)
    real(WP) ,intent(out)       :: harvest(:)
    procedure(RanVec) ,optional :: ran
    real(WP) ,allocatable       :: uniform(:)
    integer(WI)                 :: i ,sz
    real(WP) ,parameter         :: TWOPI = 2*4.0_WP*atan(1.0_WP)
#   ifdef MY_BACKTRACE
    print*, 'Entering BoxMuller in ', __FILE__, ' @ ', __LINE__, '.'
#   endif
    sz = size(harvest)
    if ( sz == 0 ) return
    allocate(uniform(sz + mod(sz,2)))
    if ( present(ran) ) then
       call ran(uniform)
    else
       call random_number(uniform)
    end if
    do concurrent (i = 1:sz-1:2)
       harvest(i)   = sqrt(-2*log(uniform(i)))*cos(TWOPI*uniform(i+1))
       harvest(i+1) = sqrt(-2*log(uniform(i)))*sin(TWOPI*uniform(i+1))
    end do
    if (mod(sz,2) /= 0) then
       harvest(sz) = sqrt(-2*log(uniform(sz)))*cos(TWOPI*uniform(sz+1))
    end if
#   ifdef MY_BACKTRACE
    print*, 'Leaving BoxMuller in ',__FILE__, ' @ ', __LINE__, '.'
#   endif
  end subroutine
  subroutine MarsagliaPolar(harvest,ran)
    !Box-Muller might be faster due to lack of branches
    real(WP) ,intent(out)       :: harvest(:)
    procedure(RanVec) ,optional :: ran
    real(WP) ,allocatable       :: uniform(:)
    real(WP)                    :: u(1) ,v(1) ,s(1)
    integer(WI)                 :: i ,sz
#   ifdef MY_BACKTRACE
    print*, 'Entering MarsagliaPolar in ', __FILE__, ' @ ', __LINE__, '.'
#   endif
    sz = size(harvest)
    if ( sz == 0 ) return
    allocate(uniform(sz + mod(sz,2)))
    if ( present(ran) ) then
       call ran(uniform)
    else
       call random_number(uniform)
    end if
    do i = 1,size(harvest)-1,2
       u = 2.0_WP*uniform(i)   - 1.0_WP
       v = 2.0_WP*uniform(i+1) - 1.0_WP
       s = u**2 + v**2
       do
          if(s(1) > 0.0_WP .and. s(1) < 1.0_WP) exit
          if ( present(ran) ) then
             call ran(u)
          else
             call random_number(u)
          end if
          u = 2.0_WP*u - 1.0_WP
          if ( present(ran) ) then
             call ran(v)
          else
             call random_number(v)
          end if
          v = 2.0_WP*v - 1.0_WP
          s = u**2 + v**2
       end do
       s = sqrt(-2*log(s)/s)
       harvest(i)   = u(1)*s(1)
       harvest(i+1) = v(1)*s(1)
    end do
    if( mod(sz,2) /= 0) then !odd # elem
       u = uniform(sz-1)
       v = uniform(sz)
       s = u**2 + v**2
       do
          if(s(1) > 0.0_WP .and. s(1) < 1.0_WP) exit
          if ( present(ran) ) then
             call ran(u)
          else
             call random_number(u)
          end if
          u = 2.0_WP*u - 1.0_WP
          if ( present(ran) ) then
             call ran(v)
          else
             call random_number(v)
          end if
          v = 2.0_WP*v - 1.0_WP
          s = u**2 + v**2
       end do
       s = sqrt(-2.0_WP*log(s)/s)
       harvest(size(harvest)) = u(1)*s(1)
    end if
#   ifdef MY_BACKTRACE
    print*, 'Leaving NormalDist in ', __FILE__, ' @ ', __LINE__, '.'
#   endif
  end subroutine
end module
