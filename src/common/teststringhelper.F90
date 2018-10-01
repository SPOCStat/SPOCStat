function teststringhelper(argc,argv) result(res) bind(c,name='teststringhelper')
  use iso_c_binding  ,only: c_int ,c_ptr
  use kinds_m        ,only: WI ,WP
  use stringhelper_m ,only: string_t ,len ,Int2Char ,C2Fchar ,ConcatInt !visible as a work around for //
  implicit none
  character(*) ,parameter           :: file_name=__FILE__
  integer(WI)  ,parameter           :: suff_len = 4,& !assume .f90 or .F90 extension
                                       the_meaning_of_life = 42
  integer(c_int) ,intent(in) ,value :: argc
  type(c_ptr)    ,intent(in)        :: argv(0:argc-1)
  integer(c_int)                    :: res
  type(string_t)  ,allocatable      :: arguments(:)
  class(string_t) ,allocatable      :: testvar1 ,testvar2
  integer(WI)                       :: i ,failures, found, c_len

  ! Since tests are linked with a c compiler deal with different Fortran compilers idiosyncrasies
  ! for initializing the Fortran run-time library (FRTL)
#include "fort_init_RTL.F90"

  failures = 0
  res = 0 !Start assuming no errors
  allocate(string_t :: testvar1,testvar2)
  allocate(arguments(argc))
  if ( argc > 0) then !pull arguments
     do i = 1,argc
        arguments(i) = C2FChar(argv(i-1))
     end do
     found = index(file_name,arguments(1)%Get())
     if ( found == 0 ) then !arguments(1) is not stringhelpertests
        print*, 'Argument(1) is not found in the file name. Are we building & testing with CMake/CTest?'
        res = 1
        failures = failures + 1
     end if

     c_len = len(argv(0))
     if ( c_len /= (len(file_name) - suff_len) ) then
        print*, 'Overloaded len function appears to be broken, or not testing with CMake/CTest.'
        res = 1
        failures = failures + 1
     end if

     testvar1 = argv(0)
     testvar1 = testvar1 // '.F90' ! Valgrind seems to think there is a memory leak here
     !testvar2 = testvar1%ConcatChars('.F90') ! ALSO LEAKS!!!
     !testvar1 = testvar1%ConcatChars('.F90') ! ALSO LEAKS!!!
     if ( testvar1%Get() /= file_name ) then
        print*, 'Overloaded assignment failing or wrong argument passed to stringhelpertests. &
             &Are we building and testing with CMake/CTest?'
        res = 1
        failures = failures + 1
     end if
  else !Something amiss, CTest/CMake passes file name w/o suffix as argv(0)
     print*, "No arguments passed to " // file_name // ". Are we executing the test driver under CTest?"
     res = 1
     failures = failures + 1
  end if

  testvar1 = 'How are you'
  if ( testvar1%Get() /= 'How are you' ) then
     print*, 'String overloaded assignment failing with intrinsic character.'
     res = 1
     failures = failures + 1
  end if

  testvar2 = testvar1
  if ( testvar2%Get() /= 'How are you' ) then
     print*, 'String overloaded assignment failing with intrinsic character.'
     res = 1
     failures = failures + 1
  end if

  testvar1 = ' today?'
  testvar1 = testvar2 // testvar1
  if ( testvar1%Get() /= 'How are you today?' ) then
     print*, 'Overloaded string_t concatenation failing.'
     res = 1
     failures = failures + 1
  end if

  testvar1 = the_meaning_of_life
  if ( testvar1%Get() /= '42' ) then
     print*, 'Overloaded string_t integer conversion and assignment failing.'
     res = 1
     failures = failures + 1
  end if

  testvar1 = string_t('The meaning of life is ')
  testvar1 = testvar1 // Int2Char(42_WI)
  if ( testvar1%Get() /= 'The meaning of life is 42' ) then
     print*, 'Overloaded string_t character concatenation or Int2Char function failing.'
     res = 1
     failures = failures + 1
  end if

  testvar1 = string_t('The meaning of life is ')
  testvar1 = ConcatInt(testvar1,42_WI)
! testvar1 = testvar1 // the_meaning_of_life !This will fail for ifort 14.0.0
! testvar2 = testvar1 // 42_WI               !This fails too.
  !// operator should resolve to ConcatInt.... but its not....
  if ( testvar1%GEt() /= 'The meaning of life is 42' ) then
     print*, 'Overloaded string_t integer concatenation or ConcatInt function failing.'
     res = 1
     failures = failures + 1
  end if

  if ( failures == 0 ) then
     print*, 'All of our tests passed. W00T!'
  else
     print*, 'Oh noes! ', failures, ' test(s) failed! When in doubt blame the compiler vendor!'
  end if

end function
