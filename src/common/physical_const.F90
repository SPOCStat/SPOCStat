module physical_const_m
!******************************************************************************|
! physical_const.F90: Module for globally setting physical and mathematical    |
!                     constants.                                               |
!                                                                              |
! Dialect: Fortran 2003                                                        |
!                                                                              |
! Dependancies: intrinsic module iso_fortran_env ,kinds.F90                    |
!                                                                              |
! ToDo: Add more relevant constants.                                           |
!                                                                              |
! Copyright (C) 2018 Izaak Beekman                                             |
!..............................................................................!
  use kinds_m ,only: CheckKinds ,WI ,WP
  implicit none
  public
  private :: CheckKinds ,WI ,WP

  real(WP) ,protected :: PI     = 3.141592653589793238462643383279502884197_WP
  real(WP) ,protected :: PIO2   = 1.57079632679489661923132169163975144209858_WP
  real(WP) ,protected :: PIO3   = 1.047197551196597631317786181170959026000_WP
  real(WP) ,protected :: PIO4   = 0.785398163397448278999490867136046290400_WP
  real(WP) ,protected :: PIO6   = 0.523598775598298815658893090585479512800_WP
  real(WP) ,protected :: TWOPI  = 6.283185307179586476925286766559005768394_WP
  real(WP) ,protected :: SQRT2  = 1.41421356237309504880168872420969807856967_WP

  !Euler-Mascheroni constant
  real(WP) ,parameter :: EULER  = 0.5772156649015328606065120900824024310422_WP
  ! http://physics.nist.gov/cgi-bin/cuu/Value?eqk|search_for=Boltzmann
  real(WP) ,parameter :: KBOLTZ = 1.3806504e-23_WP!J/K +- 0.000 0024e-23 J/K
  ! http://physics.nist.gov/cgi-bin/cuu/Value?na|search_for=Avagodro
  real(WP) ,parameter :: AVOGOD = 6.02214179e23_WP!1/mol +- 0.000 000 30e23 1/mol
  ! http://physics.nist.gov/cgi-bin/cuu/Value?h
  real(WP) ,parameter :: PLANCK = 6.62606957e-34_WP!Js +- 0.000 000 29e-34 Js
  ! http://physics.nist.gov/cgi-bin/cuu/Value?c
  real(WP) ,parameter :: LIGHT  = 299792458.0_WP!m/s +- 0.0 m/s
  ! http://en.wikipedia.org/wiki/Gas_constant
  ! REAL(WP) ,PARAMETER :: R0     = 8314.472_WP           ! J/(K*kmol)
  real(WP) ,parameter :: R0     = KBOLTZ*AVOGOD*1000.0_WP ! J/(K*kmol)
  ! http://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19770009539_1977009539.pdf
  real(WP) ,parameter :: M0air  = 28.9644_WP  ! U.S. '76 standard atmosphere

contains ! 1 procedure
  subroutine InitConst(err)
    integer ,optional ,intent(out) :: err
#ifdef DEBUGGING
    print*, 'Entering InitConst in __FILE__ @ line __LINE__.'
#endif
    if (present(err)) err = 0
    call CheckKinds(err)
    if (present(err) .and. (err /= 0)) return ! If err is missing check_wp will STOP
                                              ! the program.
    PI    = 4.0_WP*atan(1.0_WP)
    PIO2  = 2.0_WP*atan(1.0_WP)
    PIO3  = 4.0_WP*atan(1.0_WP)/3.0_WP
    PIO4  =        atan(1.0_WP)
    PIO6  = 2.0_WP*atan(1.0_WP)/3.0_WP
    TWOPI = 8.0_WP*atan(1.0_WP)
    SQRT2 = sqrt(2.0_WP)
#ifdef DEBUGGING
    print*, 'Leaving InitConst in __FILE__ @ line __LINE__.'
#endif
  end subroutine
end module
