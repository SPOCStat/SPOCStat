module get_urandom_m
  use kinds_m     ,only: WP ,WI
  implicit none
  integer(WI) ,parameter :: RANDLUN = 122
contains
  function GetURandom(n) result(res)
    !Mimic behavior of random_number
    integer(WI) ,intent(in) :: n
    real(WP) ,dimension(n)  :: res
    integer :: resint(n)
    open(file='/dev/urandom',unit=RANDLUN,status='old',action='read',access='stream',form='unformatted')
    read(RANDLUN) resint
    res = real(abs(resint),kind=WP)/real(huge(resint(1)),kind=WP)
    close(RANDLUN)
  end function
end module

function testspocs(argc,argv) result(res) bind(c,name='testspocs')
  use iso_c_binding       ,only: c_int ,c_ptr
  use kinds_m             ,only: WP ,WI ,BI
  use get_urandom_m       ,only: GetURandom
  use spocs_m             ,only: spocs_t
  use statN_m             ,only: statN_t
  use spocs_cov_m         ,only: spocs_cov_t
  use spocs_cov_factory_m ,only: spocs_cov_factory_t
  use covN_factory_m      ,only: covN_factory_t
!  use normal_dist_m       ,only: BoxMuller
  use assertion_m         ,only: RelativeErr ,operator(.within.) ,HaltingOff ,&
                                 AssertIdentical ,AssertEqual, Assert
  implicit none
  !Named constants
  integer(WI) ,parameter :: VECSIZE = 16384 ,IOLUN = 111
  real(WP)    ,parameter :: TOL = 0.0001_WP
  real(WP)    ,parameter :: S30(4) = [4.0_WP ,7.0_WP ,13.0_WP ,16.0_WP]  ,&
                            OFFSETS(3) = [0.0_WP ,10.0E8_WP ,10.0E9_WP]  ,& !Naive algo will fail
                            NEGCORR(6) = [-1.0 ,-22.5 ,10.0 ,-10.0 ,22.5 ,22.5] ,&
                            POSCORR(6) = [1.0_WP ,22.5_WP ,10.0_WP ,1000000010.0_WP ,22.5_WP ,22.5_WP]
  real(WP)    ,parameter :: res2(2) = [ 10.0_WP ,22.5_WP] ,& !22.5*4/3 = 30
                            res5(5) = [-10.0_WP ,22.5_WP ,0.0_WP ,688.5_WP ,0.0_WP]

  !Dummy args
  integer(c_int) ,intent(in) ,value       :: argc
  type(c_ptr)    ,intent(in)              :: argv
  integer(c_int)                          :: res
  !variables
  logical                                 :: correct
  integer(WI)                             :: i
  real(WP)                   ,allocatable :: ranx(:) ,rany(:)
  real(WP)                                :: amxds ,bmxds
  class(spocs_t)             ,allocatable :: a ,b
  class(spocs_cov_factory_t) ,allocatable :: covar_factory
  class(spocs_cov_t)         ,allocatable :: lnkdcov ,unlnkdcov
  character(:)               ,allocatable :: msg(:)

#include "../common/fort_init_RTL.F90"

  call HaltingOff()
  res = 0
  allocate(ranx(VECSIZE) ,rany(VECSIZE))
  ranx = GetURandom(VECSIZE)
  rany = GetURandom(VECSIZE)
  open(unit=IOLUN,file='uniform.csv',form='formatted')
  write(IOLUN,"(f0.20, ' ,')") (ranx(i),i=1,size(ranx))
  write(IOLUN,"(f0.20, ' ,')") (rany(i),i=1,size(rany))
  close(IOLUN)

  write(*,*) 'Instantiating spocs_t objects as statN_t...'
  allocate(a,source=statN_t(2)) !only 2nd order moments
  allocate(b,source=statN_t(5)) !3-5th order too
  write(*,*) 'Done instantiationg spocs_t objects.'

  write(*,*) 'Subtest 1: Ensuring spocs_t objects are properly initialized...'
  correct = .true.
  msg = ['spocs_t a initialization problem','spocs_t b initialization problem'] !realloc_lhs
  correct = correct .and. AssertIdentical( [0_BI,a%GetNSamples(),b%GetNSamples()] )
  correct = correct .and. AssertEqual( [a%GetMean(),b%GetMean()],[0.0_WP,0.0_WP], msg )
  correct = correct .and. AssertIdentical( [2_WI,a%GetNMoments()] )
  correct = correct .and. AssertIdentical( [5_WI,b%GetNMoments()] )
  correct = correct .and. AssertEqual( a%GetStats(),[(0.0_WP, i=1,a%GetNMoments())] )
  correct = correct .and. AssertEqual( b%GetStats(),[(0.0_WP, i=1,b%GetNMoments())] )
  correct = correct .and. AssertEqual( [a%GetMaxDState(),b%GetMaxDState()],[(huge(0.0_WP), i=1,2)] )
  if ( .not. correct ) res = res + 1
  write(*,*) 'Subtest 1 Done! No errors found initializing spocs_t objects?' ,correct

  write(*,*) 'Instantiating linked and unlinked spocs_cov_t objects as covN_t using factory pattern...'
  allocate( covN_factory_t :: covar_factory)
  call covar_factory%Create(lnkdcov,a,b)
  call covar_factory%Create(unlnkdcov)
  write(*,*) 'Done creating and initializing linked and unlinked spocs_cov_t objects.'

  write(*,*) 'Subtest 2: Ensuring spocs_cov_t objects properly initialized...'
  correct = .true.
  correct = correct .and. AssertIdentical( [0_BI,lnkdcov%GetNSamples(),unlnkdcov%GetNSamples()] )
  correct = correct .and. AssertEqual(    lnkdcov%GetCovars(),[(0.0_WP, i=1,6)]  )
  correct = correct .and. AssertEqual(  unlnkdcov%GetCovars(),[(0.0_WP, i=1,6)]  )
  correct = correct .and. AssertEqual(   [lnkdcov%GetMaxDState()],[huge(1.0_WP)] )
  correct = correct .and. AssertEqual( [unlnkdcov%GetMaxDState()],[huge(1.0_WP)] )
  if ( .not. correct ) res = res + 10
  write(*,*) 'Subtest 2 done! No errors found initializing spocs_cov_t objects?' ,correct

  write(*,*) 'Computing stats on the following vectors using scalar push operators:'
  write(*,*) 'a:' , S30
  write(*,*) 'b:' ,-S30
  do i = 1,size(S30)
     call a%PushScalar( S30(i))
     call b%PushScalar(-S30(i))
     call unlnkdcov%PushScalar(S30(i),is_u=.true.)
     call unlnkdcov%PushScalar(S30(i) + OFFSETS(2),is_u=.false.)
  end do
  write(*,*) 'Finished pushing both vectors.'

  write(*,*) 'Subtest 3: Ensuring correct number of elements pushed to all objects...'
  correct = .true.
  correct = correct .and. AssertIdentical( [int(size(S30),BI),a%GetNSamples(),b%GetNSamples()] )
  correct = correct .and. AssertIdentical( [int(size(S30),BI),lnkdcov%GetNSamples(),unlnkdcov%GetNSamples()] )
  if ( .not. correct ) res = res + 100
  write(*,*) 'Subtest 3 done! No errors found ensuring push counts?' ,correct

  write(*,*) 'Subtest 4: Ensuring correct statistics have been computed...'
  correct = .true.
  correct = correct .and. AssertEqual( [a%GetMean()],[ 10.0_WP],['a mean not within TOL of  10!'],TOL )
  correct = correct .and. AssertEqual( [b%GetMean()],[-10.0_WP],['b mean not within TOL of -10!'],TOL )
  correct = correct .and. AssertEqual( a%GetStats(),res2,tol=TOL )
  correct = correct .and. AssertEqual( b%GetStats(),res5,tol=TOL )
  correct = correct .and. Assert( [(a%GetMaxDState() < huge(1.0_WP))] )
  correct = correct .and. Assert( [(b%GetMaxDState() < huge(1.0_WP))] )
  correct = correct .and. AssertIdentical( [int(size(S30),BI),  lnkdcov%GetNSamples()] )
  correct = correct .and. AssertIdentical( [int(size(S30),BI),unlnkdcov%GetNSamples()] )
  correct = correct .and. AssertEqual(   lnkdcov%GetCovars(),NEGCORR,tol=TOL )
  correct = correct .and. AssertEqual( unlnkdcov%GetCovars(),POSCORR,tol=TOL )
  correct = correct .and. Assert(   [lnkdcov%GetMaxDState() < huge(0.0_WP)] )
  correct = correct .and. Assert( [unlnkdcov%GetMaxDState() < huge(0.0_wp)] )
  if ( .not. correct ) res = res + 1000
  write(*,*) 'Subtest 4 done! Computation of statistics was correct?' ,correct

  write(*,*) 'Subtest 5: Checking the union operator and defined assignment...'
  correct = .true.
  ! we should probably add a test that actually causes an significant update.
  a = a .U. a
  b = b .U. b
  correct = correct .and. AssertEqual( [a%GetMaxDState()],[0.0_WP])
  correct = correct .and. AssertEqual( [b%GetMaxDState()],[0.0_WP])
  correct = correct .and. AssertEqual( a%GetStats(),res2,tol=TOL )
  correct = correct .and. AssertEqual( b%GetStats(),res5,tol=TOL )
  correct = correct .and. AssertIdentical( [int(2*size(S30),BI),a%GetNSamples(),b%GetNSamples()] )
  !Try taking the union with a null set
  a = a .U. statN_t(2)
  correct = correct .and. AssertEqual( a%GetStats(),res2,tol=TOL )
  correct = correct .and. AssertIdentical( [int(2*size(S30),BI),a%GetNSamples()] )
  correct = correct .and. AssertEqual( [a%GetMaxDState()],[0.0_WP],tol=TOL )
  ! a = statN_t(2) .U. a !gfortran 4.8.2 ICE
                         !f951: ICE: in gfc_add_component_ref, at fortran/class.c:227
                         !f951: ICE: Abort trap: 6
                         !I think this is illegal fortran 2003
  ! if ( any( abs( a%GetStats() - res2 ) > TOL ) ) res = res + 1
  ! res = res + abs(a%GetNSamples() - 8)
  ! covariance objects now
  lnkdcov   =   lnkdcov .U. lnkdcov
  unlnkdcov = unlnkdcov .U. unlnkdcov
  correct = correct .and. AssertIdentical( [int(2*size(S30),BI),  lnkdcov%GetNSamples()] )
  correct = correct .and. AssertIdentical( [int(2*size(S30),BI),unlnkdcov%GetNSamples()] )
  correct = correct .and. AssertEqual(   lnkdcov%GetCovars(),NEGCORR,tol=TOL )
  correct = correct .and. AssertEqual( unlnkdcov%GetCovars(),POSCORR,tol=TOL )
  correct = correct .and. AssertEqual(   [lnkdcov%GetMaxDState()],[0.0_WP] )
  correct = correct .and. AssertEqual( [unlnkdcov%GetMaxDState()],[0.0_wp] )
  if ( .not. correct ) res = res + 10000
  write(*,*) 'Subtest 5 done! Union operator and defined assignment correct?' ,correct

  write(*,*) 'Subtest 6: Ensuring spocs IO routines correct...'
  correct = .true.
  amxds = a%GetMaxDState()
  bmxds = b%GetMaxDState()
  open(IOLUN,file='twostatobj.stream',form='unformatted',access='stream',action='write')
  call a%WriteState(IOLUN)
  call b%WriteState(IOLUN)
  close(IOLUN)
  open(IOLUN,file='twostatobj.stream',form='unformatted',access='stream',action='read')
  call a%ReadState(IOLUN)
  call b%ReadState(IOLUN)
  close(IOLUN)
  correct = correct .and. AssertEqual( a%GetStats(),res2,tol=TOL )
  correct = correct .and. AssertEqual( b%GetStats(),res5,tol=TOL )
  correct = correct .and. AssertIdentical( [int(2*size(S30),BI),a%GetNSamples()] )
  correct = correct .and. AssertIdentical( [int(2*size(S30),BI),b%GetNSamples()] )
  correct = correct .and. AssertIdentical( [2_WI,a%GetNMoments()] )
  correct = correct .and. AssertIdentical( [5_WI,b%GetNMoments()] )
  correct = correct .and. AssertEqual( [a%GetMaxDState()],[amxds],['IO error from a'] )
  correct = correct .and. AssertEqual( [b%GetMaxDState()],[bmxds],['IO error from b'] )
  if ( .not. correct ) res = res + 100000
  write(*,*) 'Subtest 6 done! IO routines correct?' ,correct

  write(*,*) 'Subtest 7: Ensuring correct push vector methods...'
  correct = .true.
  call a%PushVector(S30)
  call b%PushVector(-S30)
  call unlnkdcov%PushVector(S30,is_u=.true.)
  call unlnkdcov%PushVector((S30 + OFFSETS(2)),is_u=.false.)
  correct = correct .and. AssertEqual( a%GetStats(),res2,tol=TOL )
  correct = correct .and. AssertEqual( b%GetStats(),res5,tol=TOL )
  correct = correct .and. AssertIdentical( [int(3*size(S30),BI),a%GetNSamples(),b%GetNSamples()] )
  correct = correct .and. AssertEqual( [a%GetMaxDState()],[0.0_WP],tol=TOL )
  correct = correct .and. AssertEqual( [b%GetMaxDState()],[0.0_WP],tol=TOL )
  if ( .not. correct ) res = res + 1000000
  write(*,*) 'Subtest 7 done! Vector methods correct?' ,correct

  write(*,*) 'Subtest 8: Checking Covariance results...'
  correct = .true.
  correct = correct .and. AssertIdentical( [int(3*size(S30),BI),  lnkdcov%GetNSamples()] )
  correct = correct .and. AssertIdentical( [int(3*size(S30),BI),unlnkdcov%GetNSamples()] )
  correct = correct .and. AssertEqual(   lnkdcov%GetCovars(),NEGCORR,tol=TOL )
  correct = correct .and. AssertEqual( unlnkdcov%GetCovars(),POSCORR,tol=TOL )
  correct = correct .and. AssertEqual( [  lnkdcov%GetMaxDState()],[0.0_WP],tol=TOL )
  correct = correct .and. AssertEqual( [unlnkdcov%GetMaxDState()],[0.0_WP],tol=TOL )
  if ( .not. correct ) res = res + 10000000
  write(*,*) 'Subtest 8 done! Covariance results correct?' ,correct

  write(*,*) 'Subtest 9: Ensuring spocs_cov IO routines are correct...'
  correct = .true.
  amxds =   lnkdcov%GetMaxDState()
  bmxds = unlnkdcov%GetMaxDState()
  open(IOLUN,file='twocovobj.stream',form='unformatted',access='stream',action='write')
  call   lnkdcov%WriteState(IOLUN)
  call unlnkdcov%WriteState(IOLUN)
  close(IOLUN)
  open(IOLUN,file='twocovobj.stream',form='unformatted',access='stream',action='read')
  call   lnkdcov%ReadState(IOLUN)
  call unlnkdcov%ReadState(IOLUN)
  close(IOLUN)
  correct = correct .and. AssertIdentical( [int(3*size(S30),BI),  lnkdcov%GetNSamples()] )
  correct = correct .and. AssertIdentical( [int(3*size(S30),BI),unlnkdcov%GetNSamples()] )
  correct = correct .and. AssertEqual(   lnkdcov%GetCovars(),NEGCORR,tol=TOL )
  correct = correct .and. AssertEqual( unlnkdcov%GetCovars(),POSCORR,tol=TOL )
  correct = correct .and. AssertEqual([  lnkdcov%GetMaxDState()],[amxds])
  correct = correct .and. AssertEqual([unlnkdcov%GetMaxDState()],[bmxds])
  if ( .not. correct ) res = res + 10000000
  write(*,*) 'Subtest 9 done! Covariance IO routines are correct?' ,correct

  write(*,*) 'Subtest 10: Checking computation of condition numbers...'
  correct = .true.
  correct = correct .and. AssertEqual( [      a%EstimateKappa()],[sqrt(1 + 100/22.5_WP)],tol=TOL )
  correct = correct .and. AssertEqual( [      b%EstimateKappa()],[sqrt(1 + 100/22.5_WP)],tol=TOL )
  correct = correct .and. AssertEqual( [lnkdcov%EstimateKappa()],[(sqrt(1 + 100/22.5_WP),i=1,2)],tol=TOL )
  if ( .not. correct ) res = res + 1000000
  write(*,*) 'Subtest 10 done! Condition numbers correctly computed?' ,correct

  write(*,*) 'Subtest 11: Checking computation of range...'
  correct = .true.
  correct = correct .and. AssertEqual( a%GetRange(), [S30(1)        ,S30(size(S30))])
  correct = correct .and. AssertEqual( b%GetRange(),-[S30(size(S30)),S30(1)        ])
  correct = correct .and. AssertEqual( reshape(lnkdcov%GetRange(),[4]),&
                                       [S30(1),S30(size(S30)),-S30(size(S30)),-S30(1)] )
  correct = correct .and. AssertEqual( reshape(unlnkdcov%GetRange(),[4]),&
                                       [S30(1),S30(size(S30)),OFFSETS(2)+S30(1),OFFSETS(2)+S30(size(S30))])
  if ( .not. correct ) res = res + 1000000
  write(*,*) 'Subtest 11 done! All ranges correctly computed?' ,correct
!stress test algorithms used by picking a set with a large condition number (mu >> sigma)
end function
