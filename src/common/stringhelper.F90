module stringhelper_m
  use kinds_m ,only: WP ,WI
  implicit none
# ifndef TESTING
  private
# endif
  public :: string_t ,len ,Int2Char ,C2FChar ,ConcatInt!visible for // work around...
  type :: string_t
     private
     character(:) ,allocatable :: string
   contains
     procedure :: Get
     procedure :: Concat
     procedure :: ConcatChars
     procedure :: ConcatInt
     procedure :: Assign
     procedure :: AssignChars
     procedure :: Int2String
     procedure :: C2FString
     generic   :: operator(//)  => Concat ,ConcatChars ,ConcatInt
     generic   :: assignment(=) => Assign ,AssignChars ,C2FString ,Int2String
  end type
  interface string_t
     procedure :: StringConstructor
  end interface
  interface len
     pure function strlen(s) bind(c,name='strlen') !Steal std C library function
       use :: iso_c_binding ,only: c_ptr ,c_size_t
       implicit none
       type(c_ptr) ,intent(in) ,value :: s
       integer(c_size_t)              :: strlen
     end function
  end interface
contains
  function StringConstructor(string) result(res)
    character(*) ,intent(in) :: string
    type(string_t)           :: res
    res%string = string
  end function
  elemental subroutine Assign(lhs,rhs)
    class(string_t) ,intent(inout) :: lhs
    class(string_t) ,intent(in)    :: rhs
    lhs%string = rhs%string !realloc lhs
  end subroutine
  elemental subroutine AssignChars(lhs,rhs)
    class(string_t) ,intent(inout) :: lhs
    character(*)    ,intent(in)    :: rhs
    lhs%string = rhs
  end subroutine
  function C2FChar(c_charptr) result(res)
    use iso_c_binding ,only: c_char ,c_ptr ,c_f_pointer
    type(c_ptr) ,intent(in)               :: c_charptr
    character(:) ,allocatable             :: res
    character(kind=c_char,len=1) ,pointer :: string_p(:)
    integer(WI)                           :: i ,c_str_len
    c_str_len = len(c_charptr)
    call c_f_pointer(c_charptr,string_p,[c_str_len])
    allocate(character(c_str_len) :: res)
    do i = 1,c_str_len
       res(i:i) = string_p(i)
    end do
  end function
  subroutine C2FString(lhs,rhs)
    use iso_c_binding ,only: c_char ,c_ptr ,c_f_pointer
    class(string_t) ,intent(inout)        :: lhs
    type(c_ptr)     ,intent(in)           :: rhs
    character(kind=c_char,len=1) ,pointer :: string_p(:)
    integer(WI)                           :: i ,c_str_len
    c_str_len = len(rhs)
    call c_f_pointer(rhs,string_p,[c_str_len])
    if ( allocated(lhs%string) ) then
       if (len(lhs%string) /= c_str_len) deallocate(lhs%string)
    end if
    if ( .not. allocated(lhs%string) ) &
         allocate(character(c_str_len) :: lhs%string)
    do i = 1,c_str_len
       lhs%string(i:i) = string_p(i)
    end do
  end subroutine
  elemental function Concat(lhs,rhs) result(res)
    class(string_t) ,intent(in)  :: lhs ,rhs
    class(string_t) ,allocatable :: res
    allocate(res,mold=lhs)
    res%string = lhs%string // rhs%string
  end function
  elemental function ConcatChars(lhs,rhs) result(res)
    class(string_t) ,intent(in)  :: lhs
    character(*)    ,intent(in)  :: rhs
    class(string_t) ,allocatable :: res
    allocate(res,mold=lhs)
    res%string = lhs%string // rhs ! Valgrind seems to think that this line leaks memory.
  end function
  elemental function Get(this) result(res)
    class(string_t) ,intent(in) :: this
    character(:) ,allocatable   :: res
    res = this%string
  end function
  elemental function NoDigits(i) result(res)
    integer(WI) ,intent(in) :: i
    integer(WI)             :: res
    res = ceiling(log10(real(i,WP)),WI)
  end function
  elemental function Int2Char(i) result(res)
    integer(WI) ,intent(in)   :: i
    character(:) ,allocatable :: res
    allocate(character(NoDigits(i)) :: res)
    write(res,'(I0)') i
  end function
  elemental subroutine Int2String(lhs,rhs)
    class(string_t) ,intent(inout) :: lhs
    integer(WI)     ,intent(in)    :: rhs
    integer(WI)                    :: length
    length = NoDigits(rhs)
    if ( allocated(lhs%string) ) deallocate(lhs%string)
    allocate(character(length) :: lhs%string)
    write(lhs%string,'(I0)') rhs
  end subroutine
  elemental function ConcatInt(lhs,rhs) result(res)
    class(string_t) ,intent(in)  :: lhs
    integer(WI)     ,intent(in)  :: rhs
    class(string_t) ,allocatable :: res
    integer(WI)                  :: length
    character(:)    ,allocatable :: work
    allocate(res,mold=lhs)
    length = NoDigits(rhs)
    allocate(character(length) :: work)
    write(work,'(I0)') rhs
    res%string = lhs%string // work !realloc lhs
    deallocate(work)
  end function
end module
