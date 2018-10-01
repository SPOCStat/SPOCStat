Module debugging_m
!******************************************************************************|
! debugging.f90: Module for turning on and off global debugging. Since the     |
!                debug flag is a compile time constant, optimizing compilers   |
!                will remove the dead code associated with the if(debug_on)    |
!                statements.                                                   |
!                                                                              |
! Dialect: Fortran 90                                                          |
!                                                                              |
! Dependancies: none                                                           |
!                                                                              |
! Copyright (C) 2018 Izaak Beekman                                             |
!..............................................................................!
  Implicit None
  Logical ,Parameter :: debug_on = .True.
End Module
