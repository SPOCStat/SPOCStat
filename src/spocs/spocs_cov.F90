module spocs_cov_m
!Covariance objects observe spocs basic statistics objects
  use kinds_m ,only: WP ,WI ,BI
  implicit none
  private
  public :: spocs_cov_t
  type ,abstract :: spocs_cov_t
     private
     logical :: ifort_work_around !fixes assignment inheritance bug
   contains
     procedure(cov_op_cov)            ,deferred :: Union
     procedure(cov_op_real)           ,deferred :: AddScalar
     procedure(push_singleton_to_cov) ,deferred :: PushScalar
     procedure(cov_op_realvec)        ,deferred :: AddVector
     procedure(push_vec_to_cov)       ,deferred :: PushVector
     procedure(cov_eq_cov)            ,deferred :: Assign
     procedure(cov_write)             ,deferred :: WriteState
     procedure(cov_read)              ,deferred :: ReadState
     procedure(cov_2_bigint)          ,deferred :: GetNSamples
     procedure(cov_2_reals)           ,deferred :: GetCovars
     procedure(cov_2_real)            ,deferred :: GetMaxDState
     procedure(cov_2_real2by2)        ,deferred :: GetRange
     procedure(cov_2_real2by1)        ,deferred :: EstimateKappa
     procedure(cov_2_real2by1)        ,deferred :: BoundRelError
!     procedure(cov_link_stat)         ,deferred :: Link !Would require a surrogate
!                                                        !to avoid circular module use
     generic :: operator(.U.) => Union
     generic :: assignment(=) => Assign
  end type
  abstract interface
     function cov_op_cov(lhs,rhs) result(res)
       import                                :: spocs_cov_t
       class(spocs_cov_t) ,intent(in)        :: lhs, rhs
       class(spocs_cov_t) ,allocatable       :: res
     end function
     subroutine cov_op_real(this,u,v)
       import                                :: spocs_cov_t ,WP
       class(spocs_cov_t)  ,intent(inout)    :: this
       real(WP)            ,intent(in)       :: u ,v
     end subroutine
     subroutine push_singleton_to_cov(this,float,is_u)
       import                                :: spocs_cov_t ,WP
       class(spocs_cov_t) ,intent(inout)     :: this
       real(WP)           ,intent(in)        :: float
       logical            ,intent(in)        :: is_u
     end subroutine
     subroutine cov_op_realvec(this,u,v)
       import                            :: spocs_cov_t ,WP
       class(spocs_cov_t) ,intent(inout) :: this
       real(WP)           ,intent(in)    :: u(:) ,v(:)
       class(spocs_cov_t) ,allocatable   :: res
     end subroutine
     subroutine push_vec_to_cov(this,floats,is_u)
       import                                :: spocs_cov_t ,WP
       class(spocs_cov_t)     ,intent(inout) :: this
       real(WP) ,dimension(:) ,intent(in)    :: floats
       logical                ,intent(in)    :: is_u
     end subroutine
     subroutine cov_eq_cov(lhs,rhs)
       import                                :: spocs_cov_t
       class(spocs_cov_t) ,intent(inout)     :: lhs
       class(spocs_cov_t) ,intent(in)        :: rhs
     end subroutine
     subroutine cov_write(this,lun)
       import                                :: spocs_cov_t
       class(spocs_cov_t) ,intent(in)        :: this
       integer            ,intent(in)        :: lun
     end subroutine
     subroutine cov_read(this,lun)
       import                                :: spocs_cov_t
       class(spocs_cov_t) ,intent(inout)     :: this
       integer        ,intent(in)            :: lun
     end subroutine
     function cov_2_bigint(this) result(res)
       import                                :: spocs_cov_t ,BI
       class(spocs_cov_t) ,intent(in)        :: this
       integer(BI)                           :: res
     end function
     function cov_2_reals(this) result(res)
       import                         :: spocs_cov_t ,WP
       class(spocs_cov_t) ,intent(in) :: this
       real(WP)                       :: res(6)
     end function
     function cov_2_real(this) result(res)
       import                         :: spocs_cov_t ,WP
       class(spocs_cov_t) ,intent(in) :: this
       real(WP)                       :: res
     end function
     function cov_2_real2by2(this) result(res)
       import                         :: spocs_cov_t ,WP
       class(spocs_cov_t) ,intent(in) :: this
       real(WP)                       :: res(2,2)
     end function
     function cov_2_real2by1(this) result(res)
       import                         :: spocs_cov_t ,WP
       class(spocs_cov_t) ,intent(in) :: this
       real(WP)                       :: res(2)
     end function
     ! subroutine cov_link_stat(this,u,v)
     !   import spocs_cov_t ,spocs_t
     !   class(spocs_cov_t) ,target ,intent(in)    :: this
     !   class(spocs_t)             ,intent(inout) :: u ,v
     ! end subroutine
  end interface
end module
