module assertion_m
  use ISO_FORTRAN_ENV ,only: error_unit
  use kinds_m         ,only: WI, WP ,BI
  implicit none
  logical :: halting = .true.
  private
  public :: MXERRLEN ,AssertIdentical ,Assert !,error_message_t
  public :: RelativeErr ,operator(.within.)
  public :: AssertWithinAbs ,AssertWithinRel ,AssertEqual
  public :: HaltingOn ,HaltingOff
# ifdef ERRLEN
  integer(WI) ,parameter :: MXERRLEN = ERRLEN
# else
  integer(WI) ,parameter :: MXERRLEN = 64 + 128
# endif
  ! type error_message_t
  !    character(MXERRLEN) :: string = ''
  ! end type
  interface AssertIdentical
     module procedure AssertIdenticalWI ,AssertIdenticalBI
  end interface
  interface RelativeErr
     module procedure RelErrInt ,RelErrFloat ,RelErrMixed
  end interface
  interface operator(.within.)
     module procedure WithinTol ,Within1Tol ,WithinTols
  end interface
contains
  subroutine HaltingOn(lun)
    integer ,optional ,intent(in) :: lun
    if ( present(lun) ) then
       write(lun,*) 'Halting turned on for assertions from ' // __FILE__
    else
       write(error_unit,*) 'Halting turned on for assertions from ' // __FILE__
    end if
    halting = .true.
  end subroutine
  subroutine HaltingOff(lun)
    integer ,optional ,intent(in) :: lun
    if ( present(lun) ) then
       write(lun,*) 'Halting turned off for assertions from ' // __FILE__
    else
       write(error_unit,*) 'Halting turned off for assertions from ' // __FILE__
    end if
    halting = .false.
  end subroutine
  function Assert(assertions,text,lun) result(correct)
    logical                    ,intent(in) :: assertions(:)
    character(len=*) ,optional ,intent(in) :: text(:)
    integer          ,optional ,intent(in) :: lun
    logical                                :: correct
    integer(WI)                            :: i
    integer                                :: iou
    correct = .true.
    if ( present(text) ) correct = AssertIdentical( [size(assertions) ,size(text)], lun )
    iou = error_unit
    if (present(lun)) iou = lun
    if ( .not. correct ) then
       write(iou,*) 'Error text and assertions of unequal length in Assert!'
       if ( halting ) stop 1
       return
    end if
    do i=1,size(assertions)
       if (.not. assertions(i)) then
          correct = .false.
          write(iou,*) 'Assertion ' ,i ,' of ' ,size(assertions) ,' failed with message:'
          if ( present(text) ) then
             if (trim(text(i)) /= '') then
                write(iou,*) trim(text(i))
             else
                write(iou,*) '(No message provided.)'
             end if
          else
             write(iou,*) '(No message provided.)'
          end if
       end if
    end do
    if ( halting .and. ( .not. correct ) ) then
       write(iou,*)  'Execution halted on failed assertion(s)!'
       stop 1
    end if
  end function
  function AssertIdenticalWI(integers,lun) result(identical)
    integer(WI)       ,intent(in) :: integers(:)
    integer ,optional ,intent(in) :: lun
    logical                       :: identical
    integer(WI)                   :: i
    integer                       :: iou
    identical = .true.
    iou = error_unit
    if (present(lun)) iou = lun
    do i=2,size(integers)
       if (integers(i) /= integers(1)) then
          identical = .false.
          write(iou,'(3(a,i0))') &
               'Value ',i,':',integers(i),' does not match expected value ',integers(1)
       end if
    end do
    if ( halting .and. ( .not. identical ) ) then
       write(iou,*)  'Execution halted on failed assertion!'
       stop 1
    end if
  end function
  function AssertIdenticalBI(integers,lun) result(identical)
    integer(BI)       ,intent(in) :: integers(:)
    integer ,optional ,intent(in) :: lun
    logical                       :: identical
    integer(WI)                   :: i
    integer                       :: iou
    identical = .true.
    iou = error_unit
    if (present(lun)) iou = lun
    do i=2,size(integers)
       if (integers(i) /= integers(1)) then
          identical = .false.
          write(iou,'(3(a,i0))') &
               'Value ',i,':',integers(i),' does not match expected value ',integers(1)
       end if
    end do
    if ( halting .and. ( .not. identical ) ) then
       write(iou,*) 'Execution halted on failed assertion!'
       stop 1
    end if
  end function
  elemental function RelErrFloat(measured,truth) result(res)
    real(WP) ,intent(in) :: measured ,truth
    real(WP)             :: res
    if (     truth == 0.0_WP .and. measured /= 0.0_WP ) then
       res = measured!/epsilon(measured)
    elseif ( truth == 0.0_WP .and. measured == 0.0_WP ) then
       res = 0.0_WP
    else
       res = (measured - truth)/truth
    end if
  end function
  elemental function RelErrInt(measured,truth) result(res)
    integer(WI) ,intent(in) :: measured ,truth
    real(WP)                :: res
    if (     truth == 0 .and. measured /= 0 ) then
       res = real(measured,WP)!/epsilon(1.0_WP)
    elseif ( truth == 0 .and. measured == 0 ) then
       res = 0
    else
       res = real(measured - truth,kind=WP)/truth
    end if
  end function
  elemental function RelErrMixed(measured,truth) result(res)
    real(WP)    ,intent(in) :: measured
    integer(WI) ,intent(in) :: truth
    real(WP)                :: res
    if (     truth == 0 .and. measured /= 0.0_WP ) then
       res = measured!/epsilon(1.0_WP)
    elseif ( truth == 0 .and. measured == 0.0_WP ) then
       res = 0.0_WP
    else
       res = (measured - truth)/truth
    end if
  end function
  function WithinTol(error,tol) result(res)
    real(WP) ,intent(in) :: error(:) ,tol
    logical              :: res
    res = all(abs(error) <= tol)
  end function
  function Within1Tol(error,tol) result(res)
    real(WP) ,intent(in) :: error ,tol
    logical              :: res
    res = abs(error) <= tol
  end function
  function WithinTols(error,tol) result(res)
    real(WP) ,intent(in) :: error(:) ,tol(:)
    logical              :: res
    res = AssertIdentical([size(error) ,size(tol)])
    res = res .and. all(abs(error) <= tol)
  end function
  function AssertWithinAbs(measured,truth,text,tol,lun) result(within)
    real(WP)                   ,intent(in) :: measured(:) ,truth ,tol
    character(len=*) ,optional ,intent(in) :: text(:)
    integer          ,optional ,intent(in) :: lun
    logical                                :: within
    integer                                :: iou ,i
    within = .true.
    if ( present(text) ) within = AssertIdentical([size(measured) ,size(text)])
    iou = error_unit
    if (present(lun)) iou = lun
    if ( .not. within ) then
       write(iou,*) 'Error text and assertions of unequal length in AssertWithinAbs!'
       if ( halting ) stop 1
       return
    end if
    do i = 1,size(measured)
       if ( .not. ((measured(i) - truth) .within. tol) ) then
          within = .false.
          write(iou,*) 'Value ' ,i ,',' ,measured(i) ,', not within ' ,tol ,' of ' ,truth ,':'
          if ( present(text) ) then
             if (trim(text(i)) /= '') then
                write(iou,*) text(i)
             else
                write(iou,*) '(No message provided.)'
             end if
          else
             write(iou,*) '(No message provided.)'
          end if
       end if
    end do
    if ( halting .and. ( .not. within ) ) then
       write(iou,*) 'Execution halted on failed absolute tolerance!'
       stop 1
    end if
  end function
  function AssertWithinRel(measured,truth,text,tol,lun) result(within)
    real(WP)                   ,intent(in) :: measured(:) ,truth ,tol
    character(len=*) ,optional ,intent(in) :: text(:)
    integer          ,optional ,intent(in) :: lun
    logical                                :: within
    integer                                :: iou ,i
    within = .true.
    if ( present(text) ) within = AssertIdentical([size(measured) ,size(text)])
    iou = error_unit
    if (present(lun)) iou = lun
    if ( .not. within ) then
       write(iou,*) 'Error text and assertions of unequal length in AssertWithinRel!'
       if ( halting ) stop 1
       return
    end if
    do i = 1,size(measured)
       if ( .not. (RelativeErr(measured(i),truth) .within. tol) ) then
          within = .false.
          write(iou,*) 'Relative error of value' ,i ,' more than ' ,tol ,' from ' ,truth ,':'
          if ( present(text) ) then
             if (trim(text(i)) /= '') then
                write(iou,*) text(i)
             else
                write(iou,*) '(No message provided.)'
             end if
          else
             write(iou,*) '(No message provided.)'
          end if
       end if
    end do
    if ( halting .and. ( .not. within ) ) then
       write(iou,*) 'Execution halted on failed relative tolerance!'
       stop 1
    end if
  end function
  function AssertEqual(lhs,rhs,text,tol,lun) result(equal)
    real(WP)                   ,intent(in) :: lhs(:) ,rhs(:)
    character(len=*) ,optional ,intent(in) :: text(:)
    real(WP)         ,optional ,intent(in) :: tol
    integer          ,optional ,intent(in) :: lun
    logical                                :: equal
    integer                                :: iou
    equal = .true.
    iou = error_unit
    if (present(lun)) iou = lun
    if ( present(text) ) then
       equal = AssertIdentical([size(lhs),size(rhs),size(text)] ,lun)
    else
       equal = AssertIdentical([size(lhs),size(rhs)]             ,lun)
    end if
    if ( .not. equal ) then
       write(iou,*) 'lhs and rhs or text of unequal length within AssertEqual!'
       if ( halting ) stop 1
       return
    end if
    if ( present(tol) ) then
       equal = AssertWithinRel((lhs - rhs),0.0_WP,text,tol,lun)
    else
       equal = Assert((lhs == rhs),text,lun)
    end if
  end function
end module
