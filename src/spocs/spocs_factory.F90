module spocs_factory_m
  use kinds_m ,only: WI
  use spocs_m ,only: spocs_t
  implicit none
  private
  public :: spocs_factory_t
  type ,abstract :: spocs_factory_t
   contains
     procedure(create_interface) :: create
  end type
  abstract interface
     function create_interface(this,order) result(res)
       import :: spocs_t ,spocs_factory_t
       class(spocs_factory_t) ,intent(in) :: this
       integer(WI)            ,intent(in) :: order
       class(spocs_t) ,pointer            :: res
     end function
  end interface
end module
