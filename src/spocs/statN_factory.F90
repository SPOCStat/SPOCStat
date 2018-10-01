module statN_factory_m
  use spocs_factory_m ,only: spocs_factory_t
  use spocs_m         ,only: spocs_t
  implicit none
  private
  public :: statN_factory_t
  type ,extends(spocs_factory_t) :: statN_factory_t
   contains
     procedure :: create => new_statN
  end type
contains
  function new_statN(this,order) result(res)
    class(statN_factory_t) ,intent(in) :: this
    integer(WI)            ,intent(in) :: order
    class(spocs_t), pointer            :: res
