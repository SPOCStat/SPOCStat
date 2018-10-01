module covN_factory_m
  use spocs_cov_factory_m ,only: spocs_cov_factory_t
  use spocs_m             ,only: spocs_t
  use covN_m              ,only: covN_t
  use spocs_cov_m         ,only: spocs_cov_t
  implicit none
  private
  public :: covN_factory_t
  type ,extends(spocs_cov_factory_t) :: covN_factory_t
     private
     logical :: ifort_fix2 = .true.
   contains
     procedure ,private :: CreateUnlinked => MakeCovNUnlinked
     procedure ,private :: CreateLinked   => MakeCovNLinked
  end type
contains
  subroutine MakeCovNUnlinked(this,cov)
    !This is unused but we pass it anyway to freeze the interface so it can be extended
    class(covN_factory_t)           ,intent(in)  :: this
    class(spocs_cov_t) ,allocatable ,intent(out) :: cov
    allocate(covN_t :: cov)
    !cov = covN_t()
  end subroutine
  subroutine MakeCovNLinked(this,cov,u,v)
    !This is unused but we pass it anyway to freeze the interface so it can be extended
    class(covN_factory_t)                   ,intent(in)    :: this
    class(spocs_cov_t) ,allocatable ,target ,intent(out)   :: cov
    class(spocs_t)                          ,intent(inout) :: u,v
    allocate(covN_t :: cov)
    !cov = covN_t()
    call u%RegisterCov(cov,is_u=.true.)
    call v%RegisterCov(cov,is_u=.false.)
  end subroutine
end module
