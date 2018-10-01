function testtemplate(argc,argv) result(res) bind(c,name='testtemplate')
  use iso_c_binding ,only: c_int ,c_ptr
  implicit none
  integer(c_int) ,intent(in) ,value :: argc
  type(c_ptr)    ,intent(in)        :: argv(0:argc-1) !Use c index convention
  integer(c_int)                    :: res
  type :: string !Type for arrays of pointers to var length strings
     character(:), allocatable :: item
  end type string
  type(string)                      :: arguments(1:argc) !Use Fortran index convention
  integer                           :: i

  ! Since tests are linked with a c compiler deal with different Fortran compilers idiosyncrasies
  ! for initializing the Fortran run-time library (FRTL)
#include "fort_init_RTL.F90"

  if (argc > 0) then !argv has something useful
     do i = 1, argc !make arguments accessible
        call c_f_string(argv(i-1) ,arguments(i)%item)
     end do
  end if
  print*, 'Your command line was:'
  print*, (arguments(i)%item//' ', i=1,size(arguments))
  res = 0
contains
  !Copy a null terminated C string (specified via a non-null c_ptr) to an
  !allocatable deferred length default character variable.
  subroutine c_f_string(c_string,f_string)
    use :: iso_c_binding ,only: c_char ,c_f_pointer
    type(c_ptr)  ,intent(in)               :: c_string
    character(:) ,intent(out) ,allocatable :: f_string
    character(kind=c_char)    ,pointer     :: string_ptr(:)
    integer                                :: i
    interface
       ! Steal std C library function rather than writing our own.
       function strlen(s) bind(c,name='strlen')
         use :: iso_c_binding ,only: c_ptr ,c_size_t
         implicit none
         type(c_ptr) ,intent(in) ,value :: s
         integer(c_size_t)              :: strlen
       end function strlen
    end interface
    call c_f_pointer(c_string,string_ptr,[strlen(c_string)])
    ! Allocate fortran character variable to the c string's length
    allocate(character(size(string_ptr)) :: f_string)
    ! Copy across (with possible kind conversion) characters
    forall (i = 1:size(string_ptr)) f_string(i:i) = string_ptr(i)
  end subroutine
end function
