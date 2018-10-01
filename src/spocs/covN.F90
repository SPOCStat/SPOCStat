module covN_m
  use kinds_m       ,only: WP ,WI ,BI ,BIGGEST_INT
  use spocs_m       ,only: spocs_t
  use spocs_cov_m   ,only: spocs_cov_t
  implicit none
  private
  public :: covN_t
  type ,extends(spocs_cov_t) :: covN_t
     private
     integer(BI) :: n = 0
     real(WP)    :: umean = 0.0_WP ,vmean = 0.0_WP ,covar = 0.0_WP ,&
                    varu  = 0.0_WP ,varv = 0.0_WP
     real(WP)    :: dstate(5)  = huge(0.0_WP) !umean ,vmean ,covar ,varu ,varv
     real(WP)    :: minu =  huge(1.0_WP) ,minv =  huge(1.0_WP) ,&
                    maxu = -huge(1.0_WP) ,maxv = -huge(1.0_WP)
     logical     :: have_u = .false. ,have_v = .false. ,have_uvec = .false. ,have_vvec = .false.
     real(WP)    :: u_buffer ,v_buffer
     real(WP) ,allocatable :: uvec_buff(:) ,vvec_buff(:)
     !type(statN_t) ,pointer :: u => null() ,v => null() !not needed
   contains
     procedure :: Union         => ReduceCov
     procedure :: AddScalar     => AddSingletonCov
     procedure :: PushScalar    => PushSingletonCov
     procedure :: AddVector     => ConsumeVecCov
     procedure :: PushVector    => PushVecCov
     procedure :: Assign        => CopyCov
     procedure :: WriteState    => WriteCov
     procedure :: ReadState     => ReadCov
     procedure :: GetNSamples   => SampleSizeCov
     procedure :: GetCovars     => OutputCovarN
     procedure :: Link          => LinkCov
     procedure :: GetMaxDState  => MaxDelta
     procedure :: GetRange      => CovNRange
     procedure :: EstimateKappa => EstimateConditionNumber
     procedure :: BoundRelError => EstimateRelativeErrorBound
  end type
  real(WP) ,parameter :: TOL = sqrt(epsilon(1.0_WP))
  ! interface covN_t
  !    procedure CreateAndLinkCov
  ! end interface
contains

  ! function CreateAndLinkCov(u,v)
  !   class(spocs_cov_t) ,allocatable ,target ,intent(out)   :: cov_obj
  !   class(spocs_t)                          ,intent(inout) :: u ,v
  !   allocate(covN_t :: cov_obj)
  !   select type (cov_obj)
  !   class is (covN_t)
  !      cov_obj%n        = 0
  !      cov_obj%umean = 0
  !      cov_obj%vmean = 0
  !      cov_obj%varu = 0
  !      cov_obj%varv = 0
  !   end select
  !   call cov_obj%link(u,v)
  ! end subroutine

  subroutine LinkCov(this,u,v)
    class(covN_t) ,target ,intent(in)    :: this !Actual arg must have pointer or target att.
    class(spocs_t)        ,intent(inout) :: u ,v
    call u%RegisterCov(this,is_u=.true. )
    call v%RegisterCov(this,is_u=.false.)
  end subroutine
  function ReduceCov(lhs,rhs) result(res)
    class(covN_t)      ,intent(in)  :: lhs
    class(spocs_cov_t) ,intent(in)  :: rhs
    class(spocs_cov_t) ,allocatable :: res
    class(covN_t)      ,allocatable :: local_res
    real(WP)                        :: du21 ,dv21 ,n1on ,n2on
    integer(BI)                     :: n1 ,n2 ,n
#   ifdef DEBUGGING
    print*, 'Entering ReduceCov in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
    allocate(local_res,source=lhs)
    select type(rhs)
    class is(covN_t)
       !Preconditions
       !Both null sets?
       if (lhs%n == 0 .and. rhs%n == 0) then
          call move_alloc(from=local_res,to=res)
          return
       end if
       !Integer overflow?
       if ((BIGGEST_INT - lhs%n) < rhs%n) then
          if (rhs%n > lhs%n) then
             deallocate(local_res)
             allocate(local_res,source=rhs)
          end if
          call move_alloc(local_res,res)
          ! error handling with state variable
!#         ifdef DEBUGGING
          print*, 'Bailing due to overflow in ReduceCov in ' // __FILE__ // '.'
          print*, 'Set has' ,local_res%n ,'elements out of a possible' ,BIGGEST_INT ,'elements.'
          stop 'Overflow detected.'
!#         endif
          return
       end if
       n1 = lhs%n
       n2 = rhs%n
       n = n1 + n2
       n1on = real(n1,WP)/real(n,WP)
       n2on = real(n2,WP)/real(n,WP)
       local_res%n = n
       du21            = rhs%umean - lhs%umean
       dv21            = rhs%vmean - lhs%vmean
       local_res%umean = lhs%umean + n2*du21/n
       local_res%vmean = lhs%vmean + n2*dv21/n
       n = n*n
       local_res%covar = n1on*lhs%covar + n2on*rhs%covar + n1*n2*du21*dv21/real(n,WP)
       local_res%varu  = n1on*lhs%varu  + n2on*rhs%varu  + n1*n2*du21*du21/real(n,WP)
       local_res%varv  = n1on*lhs%varv  + n2on*rhs%varv  + n1*n2*dv21*dv21/real(n,WP)
       local_res%minu  = min(rhs%minu,lhs%minu)
       local_res%minv  = min(rhs%minv,lhs%minv)
       local_res%maxu  = max(rhs%maxu,lhs%maxu)
       local_res%maxv  = max(rhs%maxv,lhs%maxv)
       if ( n1 >= n2 ) then
          local_res%dstate(1) = local_res%umean - lhs%umean
          local_res%dstate(2) = local_res%vmean - lhs%vmean
          local_res%dstate(3) = local_res%covar - lhs%covar
          local_res%dstate(4) = local_res%varu  - lhs%varu
          local_res%dstate(5) = local_res%varv  - lhs%varv
       else
          local_res%dstate(1) = local_res%umean - rhs%umean
          local_res%dstate(2) = local_res%vmean - rhs%vmean
          local_res%dstate(3) = local_res%covar - rhs%covar
          local_res%dstate(4) = local_res%varu  - rhs%varu
          local_res%dstate(5) = local_res%varv  - rhs%varv
       end if
!#      ifdef DEBUGGING
    class default
       stop "Unsupported type for rhs passed to ReduceCov!"
!#      endif
    end select
    call move_alloc(from=local_res,to=res)
#   ifdef DEBUGGING
    print*, 'Leaving ReduceCov in ' // __FILE__ // ' @ line ' // __FILE__ // '.'
#   endif
  end function
  subroutine AddSingletonCov(this,u,v)
    class(covN_t) ,intent(inout) :: this
    real(WP)      ,intent(in)    :: u ,v
    real(WP)                     :: du ,dv ,n1on2 ,ninv
    integer(BI)                  :: n
#   ifdef DEBUGGING
    print*, 'Entering AddSingletonCov in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
    if ( this%n == BIGGEST_INT ) then
!#      ifdef DEBUGGING
       print*, 'Bailing due to overflow in AddSingletonCov in ' // __FILE__ // '.'
       print*, 'Set has' ,this%n ,'elements out of a possible' ,BIGGEST_INT ,'elements.'
       stop 'Overflow detected.'
!#      endif
       return
    end if
    du             = u           - this%umean
    dv             = v           - this%vmean
    n              = this%n      + 1
    ninv           = 1.0_WP/real(n,WP)
    n1on2          = real(this%n,WP)/real(n*n,WP)
    this%n         = n
    this%dstate(1) = du*ninv
    this%dstate(2) = dv*ninv
    this%umean     = this%umean  + this%dstate(1)
    this%vmean     = this%vmean  + this%dstate(2)
    this%dstate(3) = n1on2*du*dv - this%covar*ninv
    this%covar     = this%covar  + this%dstate(3)
    this%dstate(4) = n1on2*du*du - this%varu*ninv
    this%varu      = this%varu   + this%dstate(4)
    this%dstate(5) = n1on2*dv*dv - this%varv*ninv
    this%varv      = this%varv   + this%dstate(5)
    this%minu      = min(this%minu,u)
    this%minv      = min(this%minv,v)
    this%maxu      = max(this%maxu,u)
    this%maxv      = max(this%maxv,v)
#   ifdef DEBUGGING
    print*, 'Leaaving AddSingletonCov in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
  end subroutine
  subroutine PushSingletonCov(this,float,is_u)
    class(covN_t) ,intent(inout) :: this
    real(WP)      ,intent(in)    :: float
    logical       ,intent(in)    :: is_u
#   ifdef DEBUGGING
    print*, 'Entering PushSingletonCov in '// __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
    if ( (this%have_u .and. is_u) .or. (this%have_v .and. (.not. is_u))) then
       print*, 'The same variable was pushed twice to a covar object.'
       print*, 'You must alternate which variable you push to covar objects.'
       stop 'Double push'
    end if
    if ( is_u ) then
       this%u_buffer = float
       this%have_u = .true.
    else
       this%v_buffer = float
       this%have_v = .true.
    end if
    if ( this%have_u .and. this%have_v ) then
       call this%AddScalar(this%u_buffer,this%v_buffer)
       this%have_u = .false.
       this%have_v = .false.
    end if
#   ifdef DEBUGGING
    print*, 'Leaving PushSingletonCov in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
  end subroutine
  subroutine ConsumeVecCov(this,u,v)
    class(covN_t) ,intent(inout) :: this
    real(WP)      ,intent(in)    :: u(:) ,v(:)
    real(WP)                     :: du ,dv ,n1on
    integer(BI)                  :: n
    integer(WI)                  :: i
#   ifdef DEBUGGING
    print*, 'Entering ConsumeVecCov in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
    !preconditions
    !Vector lengths match
    if (size(u) /= size(v)) &
         stop "The size of the input vectors in ConsumeVecCov don't match."
    !Null sets
    if (size(u) == 0) return !No work to be done
    !Integer overflow
    if ( (BIGGEST_INT - this%n) < size(u) ) then
!#      ifdef DEBUGGING
       print*, 'Bailing due to overflow in ConsumeVecCov in ' // __FILE__ // '.'
       print*, 'Set has' ,this%n ,'elements out of a possible' ,BIGGEST_INT ,'elements.'
       stop 'Overflow detected.'
!#      endif
       return
    end if
    this%dstate = [this%umean ,this%vmean ,this%covar ,this%varu ,this%varv]
    do i=1,size(u)
       n          = this%n          + 1
       n1on       = real(this%n,WP)/real(n,WP)
       du         = u(i)            - this%umean
       dv         = v(i)            - this%vmean
       this%umean = this%umean      + du/real(n,WP)
       this%vmean = this%vmean      + dv/real(n,WP)
       this%n     = n
       this%varu  = n1on*this%varu  + n1on*du*du/real(n,WP)
       this%varv  = n1on*this%varv  + n1on*dv*dv/real(n,WP)
       this%covar = n1on*this%covar + n1on*du*dv/real(n,WP)
       this%minu  = min(this%minu,u(i))
       this%minv  = min(this%minv,v(i))
       this%maxu  = max(this%maxu,u(i))
       this%maxv  = max(this%maxv,v(i))
    end do
    this%dstate = [this%umean ,this%vmean ,this%covar ,this%varu ,this%varv] - this%dstate
#   ifdef DEBUGGING
    print*, 'Leaving ConsumeVecCov in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
  end subroutine
  subroutine PushVecCov(this,floats,is_u)
    class(covN_t) ,intent(inout) :: this
    real(WP)      ,intent(in)    :: floats(:)
    logical       ,intent(in)    :: is_u
#   ifdef DEBUGGING
    print*, 'Entering PushVecCov in ' // __FILE__ // ' @ line ' ,__LINE__ ,'.'
#   endif
    if ( (this%have_uvec .and. is_u) .or. (this%have_vvec .and. (.not. is_u))) then
       print*, 'The same variable was pushed twice to a covar object.'
       print*, 'You must alternate which variable you push to covar objects.'
       stop 'Double push'
    end if
    if ( is_u ) then
       if ( allocated(this%uvec_buff) ) then
          if ( size(this%uvec_buff) /= size(floats) ) deallocate(this%uvec_buff)
       end if
       if ( .not. allocated(this%uvec_buff) ) allocate(this%uvec_buff(size(floats)))
       this%uvec_buff(:) = floats
       this%have_uvec = .true.
    else
       if ( allocated(this%vvec_buff) ) then
          if ( size(this%vvec_buff) /= size(floats) ) deallocate(this%vvec_buff)
       end if
       if ( .not. allocated(this%vvec_buff) ) allocate(this%vvec_buff(size(floats)))
       this%vvec_buff = floats
       this%have_vvec = .true.
    end if
    if ( this%have_uvec .and. this%have_vvec ) then
       if (size(this%uvec_buff) /= size(this%vvec_buff)) stop 'Vectors pushed to covar objects must be same length.'
       call this%AddVector(this%uvec_buff,this%vvec_buff)
       this%have_uvec = .false.
       this%have_vvec = .false.
    end if
#   ifdef DEBUGGING
    print*, ' Leaving PushVecCov in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
  end subroutine
  subroutine CopyCov(lhs,rhs)
    class(covN_t)      ,intent(inout) :: lhs
    class(spocs_cov_t) ,intent(in)    :: rhs
#   ifdef DEBUGGING
    print*, 'Entering CopyCov in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
    select type (rhs)
    class is (covN_t)
#      ifdef DEBUGGING
       if ( rhs%n < 0_BI ) then
          print*, 'Integer overflow found in rhs of defined assignment in ' // &
               __FILE__ // ' @ line ' , __LINE__ ,'.'
          print*, 'Bad rhs%n = ' ,rhs%n
          stop 'Integer overflow detected in assignment.'
       end if
#      endif
       lhs%n      = rhs%n
       lhs%umean  = rhs%umean
       lhs%vmean  = rhs%vmean
       lhs%covar  = rhs%covar
       lhs%varu   = rhs%varu
       lhs%varv   = rhs%varv
       lhs%dstate = rhs%dstate
       lhs%minu   = rhs%minu
       lhs%minv   = rhs%minv
       lhs%maxu   = rhs%maxu
       lhs%maxv   = rhs%maxv
    class default
    stop "Unsupported type rhs passed to defined assignment CopyCov."
    end select
#   ifdef DEBUGGING
    print*, 'Leaving CopyCov in ' // __FILE__ // ' @ line ' , __LINE__ ,'.'
#   endif
  end subroutine
  subroutine WriteCov(this,lun)
    class(covN_t) ,intent(in) :: this
    integer       ,intent(in) :: lun
    write(lun) this%n
    write(lun) this%covar ,this%umean ,this%vmean ,this%varu ,this%varv
    write(lun) this%dstate
    write(lun) this%minu ,this%minv ,this%maxu ,this%maxv
  end subroutine
  subroutine ReadCov(this,lun)
    class(covN_t) ,intent(inout) :: this
    integer       ,intent(in)    :: lun
    read(lun) this%n
    read(lun) this%covar ,this%umean ,this%vmean ,this%varu ,this%varv
    read(lun) this%dstate
    read(lun) this%minu ,this%minv ,this%maxu ,this%maxv
  end subroutine
  function SampleSizeCov(this) result(res)
    class(covN_t) ,intent(in) :: this
    integer(BI)               :: res
    res = this%n
  end function
  function OutputCovarN(this) result(res)
    ![Pearson corr coef, covariance, umean, vmean, uvar, vvar]
    class(covN_t) ,intent(in) :: this
    real(WP)                  :: res(6)

    if (this%varu*this%varv > 0.0_WP) then
       res(1) = this%covar/sqrt(this%varu*this%varv)
    else
       if (this%covar == 0.0_WP) then
          res(1) = 0
       else
          res(1) = sign(huge(this%covar),this%covar)
       end if
    end if

    if ( this%n == 0 .or. this%n == 1) res(1) = this%covar
    res(2:) = [this%covar ,this%umean ,this%vmean ,this%varu ,this%varv]
  end function
  function MaxDelta(this) result(res)
    class(covN_t) ,intent(in) :: this
    real(WP)                  :: res ,tmp(5)
    where(abs([this%covar ,this%umean ,this%vmean ,this%varu ,this%varv]) > TOL)
       tmp = this%dstate/[this%covar ,this%umean ,this%vmean ,this%varu ,this%varv]
    elsewhere
       tmp = this%dstate
    end where
    res = maxval(abs(tmp))
  end function
  function CovNRange(this) result(res)
    class(covN_t) ,intent(in) :: this
    real(WP)                  :: res(2,2)
    res = reshape([this%minu ,this%maxu ,this%minv ,this%maxv],[2,2])
  end function
  function EstimateConditionNumber(this) result(res)
    class(covN_t) ,intent(in) :: this
    real(WP)                  :: res(2)
    if (this%varu > 0.0_WP) then
       res(1) = sqrt(1.0_WP + (this%umean**2)/this%varu)
    else
       res(1) = huge(this%umean)
    end if
    if (this%varv > 0.0_WP) then
       res(2) = sqrt(1.0_WP + (this%vmean**2)/this%varv)
    else
       res(2) = huge(this%vmean)
    end if
  end function
  function EstimateRelativeErrorBound(this) result(res)
    class(covN_t) ,intent(in) :: this
    real(WP)                  :: res(2)
    res = this%n*epsilon(1.0_WP)*this%EstimateKappa()
  end function
end module
