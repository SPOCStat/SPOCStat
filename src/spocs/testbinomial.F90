function testbinomial(argc,argv) result(res) bind(c,name='testbinomial')
  use iso_c_binding ,only: c_int ,c_ptr
  use binomial_m    ,only: binomial_coeff
  implicit none
  integer ,parameter                :: N0(0:0) = [1]                                     ,&
                                       N1(0:1) = [1 ,1]                                  ,&
                                       N2(0:2) = [1 ,2  ,1]                              ,&
                                       N3(0:3) = [1 ,3  ,3  ,1]                          ,&
                                       N4(0:4) = [1 ,4  ,6  ,4  ,1]                      ,&
                                       N5(0:5) = [1 ,5  ,10 ,10 ,5   ,1]                 ,&
                                       N6(0:6) = [1 ,6  ,15 ,20 ,15  ,6   ,1]            ,&
                                       N7(0:7) = [1 ,7  ,21 ,35 ,35  ,21  ,7  ,1]        ,&
                                       N8(0:8) = [1 ,8  ,28 ,56 ,70  ,56  ,28 ,8  ,1]    ,&
                                       N9(0:9) = [1 ,9  ,36 ,84 ,126 ,126 ,84 ,36 ,9 ,1]
  integer(c_int) ,intent(in) ,value :: argc
  type(c_ptr)    ,intent(in)        :: argv
  integer(c_int)                    :: res

  ! Since tests are linked with a c compiler deal with different Fortran compilers idiosyncrasies
  ! for initializing the Fortran run-time library (FRTL)
#include "../common/fort_init_RTL.F90"

  res = 0
  if ( .not. all(N0 == binomial_coeff(0)) ) then
     print*, binomial_coeff(0)
     res = 1
  end if
  if ( .not. all(N1 == binomial_coeff(1)) ) then
     print*, binomial_coeff(1)
     res = 1
  end if
  if ( .not. all(N2 == binomial_coeff(2)) ) then
     print*, binomial_coeff(2)
     res = 1
  end if
  if ( .not. all(N3 == binomial_coeff(3)) ) then
     print*, binomial_coeff(3)
     res = 1
  end if
  if ( .not. all(N4 == binomial_coeff(4)) ) then
     print*, binomial_coeff(4)
     res = 1
  end if
  if ( .not. all(N5 == binomial_coeff(5)) ) then
     print*, binomial_coeff(5)
     res = 1
  end if
  if ( .not. all(N6 == binomial_coeff(6)) ) then
     print*, binomial_coeff(6)
     res = 1
  end if
  if ( .not. all(N7 == binomial_coeff(7)) ) then
     print*, binomial_coeff(7)
     res = 1
  end if
  if ( .not. all(N8 == binomial_coeff(8)) ) then
     print*, binomial_coeff(8)
     res = 1
  end if
  if ( .not. all(N9 == binomial_coeff(9)) ) then
     print*, binomial_coeff(9)
     res = 1
  end if
end function
