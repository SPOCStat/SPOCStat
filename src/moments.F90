module moments
  !! Defines an abstract class for accumulating arbitrary order
  !! statistical moments
!  use object, only: object
  implicit none

!  type, abstract, extends(object) :: moment_t
  type, abstract :: moment_t
     private
   contains
     procedure(self_op_self),   deferred :: union
     procedure(self_op_scalar), deferred :: self_join_scalar
     procedure(self_op_array),  deferred :: self_join_array
     generic :: operator(.U.) => union, &
          self_join_scalar, &
          self_join_array
  end type

  abstract interface
     pure function self_op_self(self,operand) result(res)
       import :: moment_t
       class(moment_t), intent(in)  :: self, operand
       class(moment_t), allocatable :: res
     end function
     pure function self_op_scalar(self,scalar) result(res)
       import :: moment_t
       class(moment_t), intent(in)  :: self
       class(*),        intent(in)  :: scalar
       class(moment_t), allocatable :: res
     end function
     pure function self_op_array(self,array) result(res)
       import :: moment_t
       class(moment_t), intent(in)  :: self
       class(*),        intent(in)  :: array(:)
       class(moment_t), allocatable :: res
     end function
  end interface
end module
