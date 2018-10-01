module kinds_m
!******************************************************************************|
! kinds.F90: Module for globally setting the kind of intrinsic types.          |
!                                                                              |
! Dialect: Fortran 2008 (named constant stop codes, ISO_FORTRAN_ENV kinds)     |
!                                                                              |
! Dependancies: intrinsic module iso_fortran_env                               |
!                                                                              |
! ToDo: Investigate IEEE floating point arithmatic intrinsic F2003 module.     |
!       Allow kinds provided in the 2008 extension of ISO_FORTRAN_ENV          |
!                                                                              |
! Copyright (C) 2018 Izaak Beekman                                             |
!..............................................................................!
  !use ISO_FORTRAN_ENV ,only: INT8 ,INT16 ,INT32 ,INT64 ,REAL32 ,REAL64 ,REAL128
  !Use some external/CMake magic to diagnose largest available integer
# ifndef BIGINT
#   define BIGINT WI
# endif
  implicit none
  private
  public :: CheckKinds ,WP ,WI ,BI ,BIGGEST_INT

  integer ,parameter :: ASC = selected_char_kind('ASCII')
! integer ,parameter :: ISO = selected_char_kind('ISO_10646') !ifort compilation error as of 14.x
  integer ,parameter :: DEF = selected_char_kind('DEFAULT')

  integer ,parameter :: INR = kind(5)
  integer ,parameter :: I4B = selected_int_kind(9) ! -10^9 < n < 10^9, 4 bytes?
  integer ,parameter :: I2B = selected_int_kind(4) ! -10^4 < n < 10^4, 2 bytes?
  integer ,parameter :: I1B = selected_int_kind(2) ! -10^2 < n < 10^2, 1 byte?
  integer ,parameter :: SP  = kind(1.0)
  integer ,parameter :: DP  = kind(1.0D0)
  integer ,parameter :: R4  = selected_real_kind(p=5, r=20)  ! Usually 4  byte words
  integer ,parameter :: R8  = selected_real_kind(p=13,r=99)  ! Usually 8  byte words
  integer ,parameter :: R16 = selected_real_kind(p=16,r=500) ! (quad?) 16 byte words ?
  integer ,parameter :: SPC = kind((1.0,1.0))
  integer ,parameter :: DPC = kind((1.0D0,1.0D0))
# ifdef MY_WP
  integer ,parameter :: WP  = MY_WP ! Set the working precision based on
                                    ! required precision.
# else
  integer ,parameter :: WP  = R8    ! Default to double precision
# endif
  integer ,parameter :: WI  = INR ! Choose the integer we will use for most stuff.

  integer ,parameter :: BI = BIGINT
  integer(BI) ,parameter :: BIGGEST_INT = huge(1_BI)


  ! Error codes
  integer ,parameter :: bad_precision   = -1  ,bad_range   = -2  ,bad_both   = -3
  integer ,parameter :: precision_abort = 100 ,range_abort = 200 ,both_abort = 300


contains ! 1 procedure
  subroutine CheckKinds(rkind,ikind,err)
    use ISO_FORTRAN_ENV ,only: error_unit
    integer ,optional ,intent(in)  :: rkind ,ikind
    integer ,optional ,intent(out) :: err
    integer                        :: local_wp, local_wi
    ! Need to check that it is in the range -999999 to 999999

#ifdef DEBUGGING
    print*, 'Entering CheckKinds in __FILE__ @ line __LINE__.'
#endif
    if (present(rkind)) then
       local_wp = rkind
    else
       local_wp = WP
    end if

    if (present(ikind)) then
       local_wi = ikind
    else
       local_wi = WI
    end if

    if (present(err)) err = 0

    select case(local_wi)
    case (-1)
       ! Requested integer range unavailable
       if (present(err)) then
          err = local_wi
          return
       else
          write(error_unit,*) 'Requested integer type unavailable.'
          stop range_abort
       end if
    end select

    select case(local_wp)
    case (bad_precision)
       ! Requested precision unavailable
       if (present(err)) then
          err = local_wp
          return
       else
          write(error_unit,*) 'Requested real precision unavailable.'
          stop precision_abort
       end if
    case (bad_range)
       ! Requested exponent range unavailable
       if (present(err)) then
          err = local_wp
          return
       else
          write(error_unit,*) 'Requested real exponent range unavailable.'
          stop range_abort
       end if
    case (bad_both)
       ! Both unavailable
       if (present(err)) then
          err = local_wp
          return
       else
          write(error_unit,*) 'Requested real precision and exponent range unavailable.'
          stop both_abort
       end if
    end select
#ifdef DEBUGGING
    print*, 'Leaving check_wp in __FILE__ @ line __LINE__.'
#endif
  end subroutine
end module
