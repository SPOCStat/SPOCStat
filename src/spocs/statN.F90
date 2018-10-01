module statN_m
  !TODO: Add checks for incompatible, null sets
  !      Implement more sensitive pair-wise algo for push routines as extended type or strategy
  !C.F.: Formulas for Robust, One-Pass Parallel Computation of Covariances
  !      and Arbitrary-Order Statistical Moments
  !by Philippe Pebay, 2008
  !Sandia Report: SAND2008-6212
  !Also based on the chapter on the factory patterns in Rousson et al.
  !the Push routines are based on the observer pattern from Arjen Marcus' book
  use kinds_m       ,only: WP ,WI ,BI ,BIGGEST_INT
  use binomial_m    ,only: binomial_coeff
  use spocs_m       ,only: spocs_t
  use spocs_cov_m   ,only: spocs_cov_t
  implicit none
  private
  public :: statN_t
  type covar_list_t !Covar elements observe stat elements
     private
     class(spocs_cov_t) ,pointer :: cov => null() !could move into spocs_cov_m but public
  end type
  type ,extends(spocs_t) :: statN_t
     private
     integer(WI)        ,allocatable :: binkp(:,:) !p choose k, i.e. n!/(k!(n-k)!
     real(WP)           ,allocatable :: M(:) !mean, 2nd and higher moments
     real(WP)           ,allocatable :: dstate(:)
     real(WP)                        :: min = huge(1.0_WP) ,max = -huge(1.0_WP)
     integer(WI)                     :: p = 2 !order of statistics(>=2)
     integer(BI)                     :: n = 0 !samples computed
     type(covar_list_t) ,allocatable :: ucov(:) ,vcov(:) !u and v covar observer collection
                                                         !(aggregation)
   contains
     procedure :: Union         => Reduce
     procedure :: AddScalar     => AddSingleton
     procedure :: PushScalar    => PushSingleton
     procedure :: AddVector     => ConsumeVec
     procedure :: PushVector    => PushVec
     procedure :: Assign        => Copy
     procedure :: WriteState    => WriteStatN
     procedure :: ReadState     => ReadStatN
     procedure :: GetNMoments   => StatNOrder
     procedure :: GetNSamples   => SampleSize
     procedure :: GetMean       => StatNMean
     procedure :: GetStats      => OutputStatN
     procedure :: RegisterCov   => SubscribeCov
     procedure :: GetMaxDState  => MaxDelta
     procedure :: GetRange      => StatNRange
     procedure :: EstimateKappa => EstimateConditionNumber
     procedure :: BoundRelError => EstimateRelativeErrorBound
  end type
  real(WP) ,parameter :: TOL = sqrt(epsilon(1.0_WP))
  interface statN_t !constructors
     procedure StatNConstructor
  end interface
contains
  function StatNConstructor(order) result(res)
    integer(WI)           ,intent(in) :: order !calc 2-order statistical moments
    class(statN_t) ,allocatable       :: res
    integer(WI)                       :: temp(0:order,order) ,i
    if ( order < 2 ) stop 'Must compute at least the 2nd moment.'
    allocate(statN_t :: res)
    allocate(res%M(1:order),source=0.0_WP)
    allocate(res%dstate(order),source=huge(1.0_WP))
    if ( order > 2 ) then
       temp = 0
       do i = 2,order !Never need p=0,1
          temp(0:i,i) = binomial_coeff(i)
       end do
       allocate(res%binkp(1:order-2,2:order) ,&
            source=temp(1:order-2,2:order)) !Never need k=0
    end if
    res%p         = order
    res%n         = 0
    res%min       =  huge(1.0_WP)
    res%max       = -huge(1.0_WP)
  end function
  function Reduce(lhs,rhs) result(res)
    !Associated covar objects must be reduced by client code
    !Calculate statistical moments of the union of two sets
    class(statN_t)  ,intent(in)  :: lhs
    class(spocs_t)  ,intent(in)  :: rhs
    class(spocs_t)  ,allocatable :: res
    class(statN_t)  ,allocatable :: local_res
    real(WP)                     :: delta ,sum ,n1on ,n2on
    integer(BI)                  :: n ,n1 ,n2
    integer(WI)                  :: k ,p
#   ifdef DEBUGGING
    print*, 'Entering Reduce in '// __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
    allocate(local_res,source=lhs) !Copy object and components
    select type(rhs)
    class is (statN_t)
       !preconditions, of sorts
       !By manipulating the last term of the sum in P. Pebay's formula (2.1)
       !null sets can be treated mathematically, unless there are two
       if (lhs%n == 0 .and. rhs%n == 0 ) then
          call move_alloc(local_res,res)
          return
       end if
       !Check for incompatible lhs, rhs
       if ( lhs%p /= rhs%p ) then !incompatible objects
          print*, 'Bailing due to incompatible LHS and RHS in Reduce in ' // __FILE__ // '.'
          print*, 'LHS has ' ,lhs%p ,'moments, but RHS has ' ,rhs%p
          stop "Incompatible stat objects in Reduce"
       end if
       !Check for integer overflow
       if ( (BIGGEST_INT - lhs%n) < rhs%n ) then
          if (rhs%n > lhs%n) then !return whichever set has the most samples
!             local_res = rhs !should resolve to copy, but ifort bug DPD200249796 and DPD200249798
             deallocate(local_res)
             allocate(local_res,source=rhs)
          end if
          call move_alloc(from=local_res,to=res)
          ! Do some error handling. Make an error state variable
!#         ifdef DEBUGGING
          print*, 'Bailing due to overflow in Reduce in ' // __FILE__ // '.'
          print*, 'Set has' ,local_res%n ,'elements out of a possible' ,BIGGEST_INT ,'elements.'
          stop "Overflow detected."
!#         endif
          return
       end if
       n1               = lhs%n
       n2               = rhs%n
       n                = n1 + n2
       local_res%n      = n
       n1on             = real(n1,WP)/real(n,WP)
       n2on             = real(n2,WP)/real(n,WP)
       delta            = rhs%M(1) - lhs%M(1)
       local_res%M(1)   = lhs%M(1) + n2on*delta
       !DIR$ LOOP COUNT MAX(7), MIN(1), AVG(1)
       do p = 2,lhs%p
          sum = 0
          !DIR$ LOOP COUNT MAX(5), MIN(0), AVG(0)
          do k = 1,p-2
             sum = sum + lhs%binkp(k,p)*(delta**k)*( &
                   ((-n2on)**k)*n1on*lhs%M(p-k) + (n1on**k)*n2on*rhs%M(p-k) )
          end do
          local_res%M(p) = n1on*lhs%M(p) + n2on*rhs%M(p) + sum + &
                           ((delta/n)**p)*(n2*(n1**p) + n1*((-n2)**p))/real(n,WP)
       end do
       if (n1 >= n2 ) then
          !DIR$ LOOP COUNT MAX(8), MIN(2), AVG(2)
          local_res%dstate = local_res%M - lhs%M
       else
          !DIR$ LOOP COUNT MAX(8), MIN(2), AVG(2)
          local_res%dstate = local_res%M - rhs%M
       end if
       local_res%min=min(lhs%min,rhs%min)
       local_res%max=max(lhs%max,rhs%max)
!#   ifdef DEBUGGING
    class default
       stop "Unsupported type for rhs passed to Reduce."
!#   endif
    end select
    call move_alloc(from=local_res,to=res)
#   ifdef DEBUGGING
    print*, 'Leaving Reduce in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
  end function
  function AddSingleton(lhs,rhs) result(res)
    !Wrapper, PushSingleton will push to covar objects
    !Calculate statistical moments of a set and a singleton
    class(statN_t) ,intent(in)  :: lhs
    real(WP)       ,intent(in)  :: rhs
    class(spocs_t) ,allocatable :: res
    class(statN_t) ,allocatable :: local_res
    real(WP)                    :: delta ,sum ,n1on
    integer(BI)                 :: n ,n1
    integer(WI)                 :: k ,p
#   ifdef DEBUGGING
    print*, 'Entering AddSingleton in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
    allocate(local_res,source=lhs)
    if (local_res%n == BIGGEST_INT ) then !Bail due to overflow
!#      ifdef DEBUGGING
       print*, 'Bailing due to overflow in AddSingleton in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
       print*, 'Set has' ,local_res%n ,'elements out of a possible' ,BIGGEST_INT ,'elements.'
       stop 'Overflow detected.'
!#      endif
       return
    end if
    n1           = lhs%n
    n            = n1 + 1
    n1on         = real(n1,WP)/real(n,WP)
    local_res%n  = n
    delta        = rhs - lhs%M(1)
    local_res%dstate(1) = delta/n
    local_res%M(1) = lhs%M(1) + local_res%dstate(1)
    !DIR LOOP COUNT MIN(1), AVG(1), MAX(7)
    do p = 2,lhs%p
       sum = 0
       !DIR$ LOOP COUNT MAX(5), MIN(0), AVG(0)
       do k = 1,p-2
          sum = sum + lhs%binkp(k,p)*((-delta/n)**k)*n1on*lhs%M(p-k)
       end do
       local_res%dstate(p) = sum + ((delta/n)**p)*(n1**p + n1*((-1)**p))/real(n,WP) - lhs%M(p)/n
       local_res%M(p) = lhs%M(p) + local_res%dstate(p)
    end do
    local_res%min = min(lhs%min,rhs)
    local_res%max = max(lhs%max,rhs)
    call move_alloc(from=local_res,to=res)
#   ifdef DEBUGGING
    print*, 'Leaving AddSingleton in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
  end function
  subroutine PushSingleton(this,rhs)
    !Will push to registered covar objects
    !Calculate statistical moments of a set and a singleton
    class(statN_t) ,intent(inout) :: this
    real(WP)       ,intent(in)    :: rhs
    integer(WI)                   :: nlist
#   ifdef DEBUGGING
    print*, 'Entering PushSingleton in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
    this = this%AddScalar(rhs)
    if (allocated(this%ucov)) then
       do nlist=1,size(this%ucov)
          call this%ucov(nlist)%cov%PushScalar(rhs,is_u=.true. ) !notify cov objs
       end do
    end if
    if (allocated(this%vcov)) then
       do nlist=1,size(this%vcov)
          call this%vcov(nlist)%cov%PushScalar(rhs,is_u=.false.) !notify cov objs
       end do
    end if
#   ifdef DEBUGGING
    print*, 'Leaving PushSingleton in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
  end subroutine
  function ConsumeVec(lhs,rhs) result(res)
    !Wrapper, PushVec will also push data to registered covar objects
    !Update lhs statistics object with the set of reals in rhs.
    class(statN_t)         ,intent(in)  :: lhs
    real(wp) ,dimension(:) ,intent(in)  :: rhs
    class(spocs_t)         ,allocatable :: res
    class(statN_t)         ,allocatable :: local_res
    real(WP)                            :: delta ,sum ,n1on
    integer(BI)                         :: n ,n1
    integer(WI)                         :: i ,p ,k
#   ifdef DEBUGGING
    print*, 'Entering ConsumeVec in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
    allocate(local_res,source=lhs)
    n = local_res%n
    if (biggest_int - size(rhs) < lhs%n) then
!#     ifdef DEBUGGING
       print*, 'Bailing due to overflow in ConsumeVec in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
       print*, 'Set has' ,local_res%n ,'elements out of a possible' ,BIGGEST_INT ,'elements.'
       stop 'Overflow detected.'
!#     endif
       return
    end if
    !DIR$ LOOP COUNT MAX(2049), MIN(16), AVG(924)
    do i = 1, size(rhs)
       n1 = n
       n = n + 1
       n1on = real(n1,WP)/real(n,WP)
       delta = rhs(i) - local_res%M(1)
       local_res%M(1) = local_res%M(1) + delta/real(n,WP)
       !DIR$ LOOP COUNT MAX(7), MIN(1), AVG(1)
       do p = local_res%p,2,-1 !iterate backwards because new M4 depends on old M3,M2 etc.
          sum = 0
          !DIR$ LOOP COUNT MAX(5), MIN(0), AVG(0)
          !DIR$ NOVECTOR
          do k = 1,p-2
             sum = sum + local_res%binkp(k,p)*((-delta/n)**k)*n1on*local_res%M(p-k)
          end do
          local_res%M(p) = n1on*local_res%M(p) + sum + ((delta/n)**p)*(n1**p + n1*((-1)**p))/n
       end do
       local_res%n   = n
       local_res%min = min(lhs%min,rhs(i))
       local_res%max = max(lhs%max,rhs(i))
    end do
    if (.not. local_res%n == BIGGEST_INT) then
       local_res%dstate = local_res%M  - lhs%M
    else
#      ifdef DEBUGGING
       print*, 'Bailing due to overflow in ConsumeVec in ' // __FILE__ // '.'
       print*, 'Set has' ,local_res%n ,'elements out of a possible' ,BIGGEST_INT ,'elements.'
       stop "Overflow detected."
#      endif
    end if
    call move_alloc(from=local_res,to=res)
#   ifdef DEBUGGING
    print*, 'Leaving ConsumeVec in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
  end function
  subroutine PushVec(this,rhs)
    !Will push to registered covar objects
    !Calculate statistical moments of a set and a vector of reals
    class(statN_t) ,intent(inout)      :: this
    real(wp) ,dimension(:) ,intent(in) :: rhs
    integer(WI)                        :: nlist
#   ifdef DEBUGGING
    print*, 'Entering PushVec in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
    this = this%AddVector(rhs)
    if (allocated(this%ucov)) then
       do nlist=1,size(this%ucov)
          call this%ucov(nlist)%cov%PushVector(rhs,is_u=.true. ) !notify cov objs
       end do
    end if
    if (allocated(this%vcov)) then
       do nlist=1,size(this%vcov)
          call this%vcov(nlist)%cov%PushVector(rhs,is_u=.false.) !notify cov objs
       end do
    end if
#   ifdef DEBUGGING
    print*, 'Leaving PushVec in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
  end subroutine
  subroutine Copy(lhs,rhs)
    class(statN_t) ,intent(inout) :: lhs
    class(spocs_t) ,intent(in)    :: rhs
#   ifdef DEBUGGING
    print*, 'Entering Copy in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
    select type(rhs)
    class is(statN_t)
#      ifdef DEBUGGING
       if (rhs%n < 0_BI) then !N overflowed somewhere
          print*, 'Integer overflow found in rhs of defined assignment in ' // &
               __FILE__ // ' @ line ' , __LINE__ ,'.'
          print*, 'Bad rhs%n = ' ,rhs%n
          stop 'Integer overflow detected in assignment.'
       end if
#      endif
       if ( .not. rhs%p == lhs%p ) then !Unusual case
          lhs%p = rhs%p
          if( allocated(lhs%M) ) deallocate(lhs%M)
          allocate( lhs%M(rhs%p) ,source=rhs%M )
          if ( allocated(lhs%dstate) ) deallocate(lhs%dstate)
          allocate( lhs%dstate(rhs%p) ,source=rhs%dstate)
          if( allocated(lhs%binkp) ) deallocate(lhs%binkp)
          allocate( lhs%binkp(rhs%p,rhs%p) ,source=rhs%binkp )
       else !Assume binomial coeffs are OK
          lhs%M = rhs%M
          lhs%dstate = rhs%dstate
       end if
       lhs%n         = rhs%n
       lhs%min       = rhs%min
       lhs%max       = rhs%max
       if( allocated(lhs%ucov) ) deallocate(lhs%ucov)
       if( allocated(rhs%ucov) ) allocate(lhs%ucov(size(rhs%ucov)),source=rhs%ucov)
       if( allocated(lhs%vcov) ) deallocate(lhs%vcov)
       if( allocated(rhs%vcov) ) allocate(lhs%vcov(size(rhs%vcov)),source=rhs%vcov)
#   ifdef DEBUGGING
    class default
       stop "Unsupported type rhs passed to defined assignment Copy."
#   endif
    end select
#   ifdef DEBUGGING
    print*, 'Leaving Copy in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
  end subroutine
  subroutine WriteStatN(this,lun)
    class(statN_t) ,intent(in) :: this
    integer        ,intent(in) :: lun
    write(lun) this%p
    write(lun) this%n
    write(lun) this%M ,this%dstate
    write(lun) this%min ,this%max
    !No good way to save covar observers. Client code responsible for this
  end subroutine
  subroutine ReadStatN(this,lun)
    class(statN_t) ,intent(inout) :: this
    integer        ,intent(in)    :: lun
    integer(WI) ,allocatable      :: temp(:,:)
    integer(WI)                   :: i ,order
    read(lun) this%p
    if (allocated(this%M))      deallocate(this%m)
    if (allocated(this%dstate)) deallocate(this%dstate)
    allocate(this%M(this%p))
    allocate(this%dstate(1:this%p))
    read(lun) this%n
    read(lun) this%M ,this%dstate
    read(lun) this%min ,this%max
    !sync binomial coefficients
    if (allocated(this%binkp)) deallocate(this%binkp)
    if ( this%p > 2 ) then
       allocate(temp(0:this%p,2:this%p))
       do i = 2,this%p !Never need p=0
          temp(0:i,i) = binomial_coeff(i)
       end do
       order = this%p
       allocate(this%binkp(1:this%p-2,2:this%p) ,&
            source=temp(1:order-2,:))
       deallocate(temp)
    end if
    !User responsible for covar observers
  end subroutine
  function StatNOrder(this) result(res)
    class(statN_t) ,intent(in) :: this
    integer(WI)                :: res
    res = this%p
  end function
  function SampleSize(this) result(res)
    class(statN_t) ,intent(in) :: this
    integer(BI)                :: res
    res = this%n
  end function
  function StatNMean(this) result(res)
    class(statN_t) ,intent(in) :: this
    real(WP)                   :: res
    res = this%M(1)
  end function
  function OutputStatN(this) result(res)
    !Output raw, biased moments, no normalization etc.
    class(statN_t) ,intent(in) :: this
    real(WP) ,allocatable      :: res(:)
#   ifdef DEBUGGING
    print*, 'Entering OutputStatN in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
    allocate(res(1:this%p))
    res    = this%M
#   ifdef DEBUGGING
    print*, 'Leaving OutputStatN in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
  end function
  subroutine SubscribeCov(this,cov,is_u)
    class(statN_t)             ,intent(inout) :: this
    class(spocs_cov_t) ,target ,intent(in)    :: cov
    logical                    ,intent(in)    :: is_u
    type(covar_list_t) ,allocatable           :: tmp(:)
#   ifdef DUBUGGING
    print*, 'Entering SubscribeCov in ' // __FILE__ // ' @ line ' , __line__ ,'.'
#   endif
    if (is_u) then
       if ( allocated(this%ucov) ) then
          allocate(tmp(size(this%ucov)+1))
          tmp(1:size(this%ucov)) = this%ucov
          call move_alloc(from=tmp,to=this%ucov)
          this%ucov(size(this%ucov))%cov => cov
       else
          allocate(this%ucov(1))
          this%ucov(1)%cov => cov
       end if
    else
       if ( allocated(this%vcov) ) then
          allocate(tmp(size(this%vcov)+1))
          tmp(1:size(this%vcov)) = this%vcov
          call move_alloc(from=tmp,to=this%vcov)
          this%vcov(size(this%vcov))%cov => cov
       else
          allocate(this%vcov(1))
          this%vcov(1)%cov => cov
       end if
    end if
#   ifdef DUBUGGING
    print*, 'Leaving SubscribeCov in ' // __FILE__ // ' @ line ' , __line__ ,'.'
#   endif
  end subroutine
  function MaxDelta(this) result(res)
    class(statN_t) ,intent(in) :: this
    real(WP)                   :: res ,tmp(this%p)
    where(abs(this%M) > TOL)
       tmp = this%dstate/this%M
    elsewhere
       tmp = this%dstate
    end where
    res = maxval(abs(tmp))
  end function
  function StatNRange(this) result(res)
    class(statN_t) ,intent(in) :: this
    real(WP)                   :: res(2)
    res(1) = this%min
    res(2) = this%max
  end function
  function EstimateConditionNumber(this) result(res)
    class(statN_t) ,intent(in) :: this
    real(WP)                   :: res
    if ( .not. allocated(this%M)) then
       res = huge(0.0_WP)
    else
       if (this%M(2) > 0.0_WP .and. this%M(1) < 2*sqrt(huge(1.0_WP))) then
          res = sqrt(1.0_WP + (this%M(1)**2)/this%M(2))
       else !Inf or NaN
          res = huge(this%m(1))
       end if
    end if
  end function
  function EstimateRelativeErrorBound(this) result(res)
    class(statN_t) ,intent(in) :: this
    real(WP)                   :: res
    if ( .not. allocated(this%M) .or. this%EstimateKappa() == huge(1.0_WP)) then
       res = huge(0.0_WP)
    else
       res = this%n*epsilon(1.0_WP)*this%EstimateKappa()
    end if
  end function

  ! recursive function PairwiseConsumeVec(lhs,rhs) result(res)
  !   !TODO: move from recursion to direct loops for speed
  !   class(statN_t)         ,intent(in)  :: lhs
  !   real(wp) ,dimension(:) ,intent(in)  :: rhs
  !   class(spocs_t)         ,allocatable :: res
  !   class(statN_t)         ,allocatable :: local_res
  !   integer(WI)                         :: split
  !   allocate(local_res,source=lhs)
  !   if ( size(rhs) <= lhs%bk ) then
  !      local_res = lhs%ConsumeVec(rhs)
  !      move_alloc(from=local_res,to=res)
  !      return
  !   end if
  !   split = size(rhs)/2
  !   !Next line will call union
  !   local_res = lhs%PairwiseConsumeVec(rhs(:split)) .U. lhs%PairwiseConsumeVec(rhs(split+1:))
  !   move_alloc(from=local_res,to=res)
  ! end function PairwiseConsumeVec

end module
