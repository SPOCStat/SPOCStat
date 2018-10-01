module spocs_m
!Single point on-line convergent statistics
!Spocs objects are observed by covariance objects (observer pattern)
# ifndef BIGINT
#   define BIGINT WI
# endif
  use kinds_m     ,only: WP, WI ,BI
  use spocs_cov_m ,only: spocs_cov_t
  implicit none
  private
  public :: spocs_t
  type, abstract :: spocs_t ! single point streaming statistics abstract/virtual class
     private
     logical :: ifort_work_around !fixes assignment inheritance bug
   contains
     procedure(spocs_op_spocs)     ,deferred :: Union
     procedure(spocs_op_real)      ,deferred :: AddScalar
     procedure(spocs_push_real)    ,deferred :: PushScalar
     procedure(spocs_op_realvec)   ,deferred :: AddVector
     procedure(spocs_push_realvec) ,deferred :: PushVector
     procedure(spocs_eq_spocs)     ,deferred :: Assign
     procedure(spocs_write)        ,deferred :: WriteState !unformatted, stream
     procedure(spocs_read)         ,deferred :: ReadState  !unformatted, stream
     procedure(spocs_2_int)        ,deferred :: GetNMoments
     procedure(spocs_2_bigint)     ,deferred :: GetNSamples
     procedure(spocs_2_real)       ,deferred :: GetMean
     procedure(register_cov)       ,deferred :: RegisterCov !register observer
     procedure(spocs_output)       ,deferred :: GetStats !output mean, std dev, 3-n std moments
     procedure(spocs_2_real)       ,deferred :: GetMaxDState
     procedure(spocs_2_pair)       ,deferred :: GetRange
     procedure(spocs_2_real)       ,deferred :: EstimateKappa
     procedure(spocs_2_real)       ,deferred :: BoundRelError
     ! procedure(spocs_output)       ,deferred :: UnbiasedStats ! unbiased mean, std dev, skewness, kurtosis if present
     ! procedure(spocs_2_real)       ,deferred :: GetVariance
     generic :: operator(.U.) => Union ,AddScalar ,AddVector
     generic :: assignment(=) => Assign
  end type
  ! Polymorphic reallocate lhs is F2008 and not implemented by many compilers
  ! This is the correct way to emulate that behavior, but causes leaky code
  ! as of ifort 14.x due to a compiler bug
  ! interface assignment(=)
  !    procedure :: Copy
  ! end interface
  abstract interface
     subroutine spocs_reset(this)
       import                              :: spocs_t
       class(spocs_t) ,intent(inout)       :: this
     end subroutine
     function spocs_op_spocs(lhs,rhs) result(res)
       import                              :: spocs_t
       class(spocs_t) ,intent(in)          :: lhs, rhs
       class(spocs_t) ,allocatable         :: res
     end function
     function spocs_op_real(lhs,rhs) result(res)
       import                              :: spocs_t ,WP
       class(spocs_t)  ,intent(in)         :: lhs
       real(WP)        ,intent(in)         :: rhs
       class(spocs_t) ,allocatable         :: res
     end function
     subroutine spocs_push_real(this,rhs)
       import                              :: spocs_t ,WP
       class(spocs_t)  ,intent(inout)      :: this
       real(WP)        ,intent(in)         :: rhs
     end subroutine
     function spocs_op_realvec(lhs,rhs) result(res)
       import                              :: spocs_t ,WP
       class(spocs_t)          ,intent(in) :: lhs
       real(WP), dimension(:)  ,intent(in) :: rhs
       class(spocs_t)         ,allocatable :: res
     end function
     subroutine spocs_push_realvec(this,rhs)
       import                              :: spocs_t ,WP
       class(spocs_t)       ,intent(inout) :: this
       real(WP) ,dimension(:) ,intent(in)  :: rhs
     end subroutine
     subroutine spocs_eq_spocs(lhs,rhs)
       import                              :: spocs_t
       class(spocs_t) ,intent(inout)       :: lhs
       class(spocs_t) ,intent(in)          :: rhs
     end subroutine
     subroutine spocs_write(this,lun)
       import                              :: spocs_t
       class(spocs_t) ,intent(in)          :: this
       integer        ,intent(in)          :: lun
     end subroutine
     subroutine spocs_read(this,lun)
       import                              :: spocs_t
       class(spocs_t) ,intent(inout)       :: this
       integer        ,intent(in)          :: lun
     end subroutine
     function spocs_2_int(this) result(res)
       import                              :: spocs_t ,WI
       class(spocs_t) ,intent(in)          :: this
       integer(WI)                         :: res
     end function
     function spocs_2_bigint(this) result(res)
       import                              :: spocs_t ,BI
       class(spocs_t) ,intent(in)          :: this
       integer(BI)                         :: res
     end function
     function spocs_2_real(this) result(res)
       import                              :: spocs_t ,WP
       class(spocs_t) ,intent(in)          :: this
       real(WP)                            :: res
     end function
     function spocs_output(this) result(res)
       import                              :: spocs_t ,WP
       class(spocs_t), intent(in)          :: this
       real(WP), allocatable               :: res(:)
     end function
     subroutine register_cov(this,cov,is_u)
       import                                    :: spocs_t ,spocs_cov_t
       class(spocs_t)             ,intent(inout) :: this
       class(spocs_cov_t) ,target ,intent(in)    :: cov
       logical                    ,intent(in)    :: is_u
     end subroutine
     function spocs_2_pair(this) result(res)
       import                              :: spocs_t ,WP
       class(spocs_t) ,intent(in)          :: this
       real(WP)                            :: res(2)
     end function
  end interface
! contains
!   subroutine copy(lhs,rhs)
!     class(spocs_t), allocatable ,intent(out) :: lhs
!     class(spocs_t)              ,intent(in)  :: rhs
!     allocate(lhs,source=rhs)
!   end subroutine
end module
