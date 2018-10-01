module spocs_cov_factory_m
  use spocs_cov_m ,only: spocs_cov_t
  use spocs_m     ,only: spocs_t
  implicit none
  private
  public :: spocs_cov_factory_t
  type ,abstract :: spocs_cov_factory_t
     private
     logical :: ifort_fix1 = .true. !bug fix
   contains
     procedure(create_stand_alone) ,private ,deferred :: CreateUnlinked
     procedure(create_and_link)    ,private ,deferred :: CreateLinked
     generic :: Create => CreateUnlinked ,CreateLinked
  end type
  abstract interface
     subroutine create_stand_alone(this,cov)
       import :: spocs_cov_factory_t ,spocs_cov_t
       class(spocs_cov_factory_t)      ,intent(in)  :: this
       class(spocs_cov_t) ,allocatable ,intent(out) :: cov
     end subroutine
     subroutine create_and_link(this,cov,u,v)
       import :: spocs_cov_factory_t ,spocs_cov_t ,spocs_t
       class(spocs_cov_factory_t)              ,intent(in)    :: this
       class(spocs_cov_t) ,allocatable ,target ,intent(out)   :: cov
       class(spocs_t)                          ,intent(inout) :: u ,v
     end subroutine
  end interface
end module
