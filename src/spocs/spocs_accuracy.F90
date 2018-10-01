program spocs_accuracy
! requires -assume realloc_lhs
  !!!!!!!!!!!!!!!!!!!!!!!!
  ! USEd modules & imports
  !!!!!!!!!!!!!!!!!!!!!!!!
  use iso_fortran_env ,only: DP => real64 ,SP => real32 ,&
       stderr => error_unit ,&
       stdin  => input_unit ,&
       stdout => error_unit
  use physical_const_m ,only: R0 ,M0air
  use kinds_m ,only: WP ,WI ,BI
  use spocs_m ,only: spocs_t
  use statN_m ,only: statN_t
  use spocs_cov_m ,only: spocs_cov_t
  use spocs_cov_factory_m ,only: spocs_cov_factory_t
  use covN_factory_m ,only: covN_factory_t

  implicit none

  !!!!!!!!!!!!!!!!!!!
  ! Named constants
  !!!!!!!!!!!!!!!!!!!
  real(DP) ,parameter :: R_air = R0/M0air
  real(DP) ,parameter :: Cp = 3.5*R_air
  real(DP) ,parameter :: Cp_inv = 1.0_DP/Cp

  real(DP) ,parameter :: l2_inv = 1.0_DP/log(2.0_DP)

  integer ,parameter :: nspan = 546 ! limited by M3w case

  integer ,parameter :: order = 4!th

  ! Name the output files according to working precision so that we don't clobber them
# ifndef MY_WP
  character(len=*) ,parameter :: convergence_fl = "convergence_DP.dat"
# else
#   if MY_WP == R4
  character(len=*) ,parameter :: convergence_fl = "convergence_SP.dat"
#   else
  character(len=*) ,parameter :: convergence_fl = "convergence_DP.dat"
#   endif
# endif
  character(len=*) ,parameter :: spocs_stats_fl = "spocs_stats"
  character(len=*) ,parameter :: txtbk_stats_fl = "txtbk_stats"
  character(len=*) ,parameter :: rl_fmt = '(E16.8)'

  !!!!!!!!!!!!!!!!!!!!!
  ! type declarations
  !!!!!!!!!!!!!!!!!!!!!
  type cov_array
     class(spocs_cov_t) ,allocatable :: cov
  end type cov_array

  !!!!!!!!!!!!!!!!!!!!!!!
  ! Variable declarations
  !!!!!!!!!!!!!!!!!!!!!!!
  class(spocs_cov_factory_t)    ,allocatable :: covar_factory
  type(cov_array) ,dimension(:) ,allocatable :: uT ,uw ,rhoT ,rhou ,rhow ,up
  class(spocs_t)  ,dimension(:) ,allocatable :: u ,v ,w ,rho ,T ,p ,T0

  integer(WI) :: imx ,i ,jmx ,kmx ,k ,ibeg ,iend ,nend ,status ,nhdirs ,curhdir ,arglen
  integer(WI) :: time_lun ,output_lun ,txtbk_lun

  real(DP) :: time ,delta0 ,dstar0 ,rhoinf ,uinf ,Tinf ,Twall
  real(WP) :: NptsInv
  real(DP) ,dimension(:)   ,allocatable :: x ,y ,z ,dzdk ,dzdk2
  real(WP) ,dimension(:)   ,allocatable :: umean ,uvar ,u3rd ,u4th ,vmean ,vvar ,wmean ,wvar ,&
       pmean ,pvar ,p3rd ,p4th ,Tmean ,Tvar ,T3rd ,T4th ,rhomean ,uwcov ,uTcov ,&
       T0mean ,T0var
  real(DP) ,dimension(:,:) ,allocatable :: u2d ,v2d ,w2d ,rho2d ,T2d ,p2d
  real(WP) ,dimension(:,:) ,allocatable :: R_uT ,R_uw ,R_rhoT ,R_rhou ,R_rhow ,R_up

  character(len=:),allocatable :: h_dir

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Start executable portion
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  write(stderr,'(A,E17.9)') "Gas constant for air is: ", R_air
  write(stderr,'(A,E17.9)') "Cp for air is: ", Cp

  ! check that the list of hdirs was passed on the command line
  nhdirs = command_argument_count()
  if (nhdirs <= 0) stop "Please pass a list of H directories to process &
       &as command line arguments to this program"
  call get_arg_n(1)

  ! Read in the old_plane data file header, must come after allocate_buffers
  call read_hdr()
  write(stderr,'(4(A,I0))') 'N-Hdir: ', nhdirs, ', imax: ', imx,', jmax: ',jmx,', kmax: ',kmx
  if (jmx < nspan) stop 'PROBLEM! Make sure nspan >= jmax!'

  call allocate_buffers()

  call allocate_init_spocs() ! Must come after read_hdr

  ! Allocate and initialize variables for non-spocs algorithms
  call allocate_init_non_spocs()

  ! Set the window to use for streamwise averaging
  iend = imx  - 250
  ibeg = iend - 400

  call write_convergence_tcplt_hdr(time_lun)

  ! Loop over the H-directories passed as command line arguments
  do curhdir = 1,nhdirs
     call get_arg_n(curhdir)

     write(stderr,'(A,f6.2,A)') 'Reading '//h_dir//'. ', real(100*(curhdir - 1))/nhdirs , '% done.'

     ! Open and overwrite last tecplot file with results as they are computed
     call write_spocs_tecplot_hdr(curhdir)
     call write_txtbk_tecplot_hdr(curhdir,txtbk_lun)


     ! Read in and process each k-plane
     do k = 1,kmx
        call read_plane(k)

        do i = ibeg, iend
           ! Send the updates, one full j line at a time
           ! The mean and 2nd - 4th order moments will be updated
           ! and the data will be passed to the covariance objects
           ! too, which will in turn update all their quantities
           call   v(k)%PushVector(REAL(  v2d(:nspan,i),kind=WP))
           call   p(k)%PushVector(REAL(  p2d(:nspan,i),kind=WP))
           call   u(k)%PushVector(REAL(  u2d(:nspan,i),kind=WP))
           call   w(k)%PushVector(REAL(  w2d(:nspan,i),kind=WP))
           call rho(k)%PushVector(REAL(rho2d(:nspan,i),kind=WP))
           call   T(k)%PushVector(REAL(  T2d(:nspan,i),kind=WP))
           call  T0(k)%PushVector(tot_temp(T2d(:nspan,i),u2d(:nspan,i),v2d(:nspan,i),w2d(:nspan,i)))

           ! Also compute quantities needed for other algorithms,
           ! e.g., text book algorithm
           umean(k)   = umean(k)   + sum(u2d(  :nspan,i))
           uvar(k)    = uvar(k)    + sum(u2d(  :nspan,i)*u2d(:nspan,i))
           u3rd(k)    = u3rd(k)    + sum(u2d(  :nspan,i)**3)
           u4th(k)    = u4th(k)    + sum(u2d(  :nspan,i)**4)
           vmean(k)   = vmean(k)   + sum(v2d(  :nspan,i))
           vvar(k)    = vvar(k)    + sum(v2d(  :nspan,i)*v2d(:nspan,i))
           wmean(k)   = wmean(k)   + sum(w2d(  :nspan,i))
           wvar(k)    = wvar(k)    + sum(w2d(  :nspan,i)*w2d(:nspan,i))
           pmean(k)   = pmean(k)   + sum(p2d(  :nspan,i))
           pvar(k)    = pvar(k)    + sum(p2d(  :nspan,i)*p2d(:nspan,i))
           p3rd(k)    = p3rd(k)    + sum(p2d(  :nspan,i)**3)
           p4th(k)    = p4th(k)    + sum(p2d(  :nspan,i)**4)
           Tmean(k)   = Tmean(k)   + sum(T2d(  :nspan,i))
           Tvar(k)    = Tvar(k)    + sum(T2d(  :nspan,i)*T2d(:nspan,i))
           T3rd(k)    = T3rd(k)    + sum(T2d(  :nspan,i)**3)
           T4th(k)    = T4th(k)    + sum(T2d(  :nspan,i)**4)
           rhomean(k) = rhomean(k) + sum(rho2d(:nspan,i))
           uTcov(k)   = uTcov(k)   + sum(u2d(  :nspan,i)*T2d(:nspan,i))
           uwcov(k)   = uwcov(k)   + sum(u2d(  :nspan,i)*w2d(:nspan,i))
           T0mean(k)  = T0mean(k)  + sum( &
                tot_temp(T2d(:nspan,i),u2d(:nspan,i),v2d(:nspan,i),w2d(:nspan,i)))
           T0var(k)   = T0var(k) + sum( &
                ( tot_temp(T2d(:nspan,i),u2d(:nspan,i),v2d(:nspan,i),w2d(:nspan,i)) )**2 )

        end do
        write(time_lun,'(I0)') u(k)%GetNSamples()
        write(time_lun,rl_fmt) z(k), time, &
              u(k)%GetStats(),  u(k)%EstimateKappa(),  u(k)%BoundRelError(), &
              T(k)%GetStats(),  T(k)%EstimateKappa(),  T(k)%BoundRelError(), &
              p(k)%GetStats(),  p(k)%EstimateKappa(),  p(k)%BoundRelError(), &
             t0(k)%GetStats(), T0(k)%EstimateKappa(), T0(k)%BoundRelError()

        ! store the covariance, correlation coefficient and other stuff in arrays
        ! so we can extract what we want: [corr-coeff, covariance].
        R_uT(:,k)   = uT(k)%cov%GetCovars()
        R_uw(:,k)   = uw(k)%cov%GetCovars()
        R_rhoT(:,k) = rhoT(k)%cov%GetCovars()
        R_rhou(:,k) = rhou(k)%cov%GetCovars()
        R_rhow(:,k) = rhow(k)%cov%GetCovars()
        R_up(:,k)   = up(k)%cov%GetCovars()

        ! Let's write out the file as each time we visit a new snapshot, that way we can look
        ! at it while we wait and as it converges
        open(newunit=output_lun,file=pick_fl(spocs_stats_fl,curhdir,WP),status="old",position='append')
        write(output_lun,rl_fmt) z(k)
        write(output_lun,'(I0)') u(k)%GetNSamples()
        write(output_lun,rl_fmt) u(k)%GetStats() ,v(k)%GetStats() ,w(k)%GetStats() ,&
             p(k)%GetStats() ,T(k)%GetStats() ,rho(k)%GetStats() ,T0(k)%GetStats() ,&
             R_uT(1:2,k) ,R_uw(1:2,k) ,R_rhoT(1:2,k), R_rhou(1:2,k) ,&
             R_rhow(1:2,k) ,R_up(1:2,k)
        close(output_lun)
     end do
     ! output textbook stats each hdirectory
     call write_txtbk_tecplot_fl(txtbk_lun,kmx)

  end do

  close(time_lun)

  ! second program will read in this file and compute the statistics in the usual way,
  ! as well as compute the double precision SPOCS truth data

contains

  subroutine allocate_buffers()
    allocate(u2d(jmx,imx),v2d(jmx,imx),w2d(jmx,imx),rho2d(jmx,imx),T2d(jmx,imx),p2d(jmx,imx))
    allocate(R_uT(6,kmx),R_uw(6,kmx),R_rhoT(6,kmx),R_rhou(6,kmx),R_rhow(6,kmx),R_up(6,kmx))
  end subroutine allocate_buffers

  subroutine allocate_init_spocs()
    ! Instantiate the spocs_t objects as covN_t, with up to 4th order moments
    allocate(  u(kmx),source=statN_t(order=order))
    allocate(  v(kmx),source=statN_t(order=order))
    allocate(  w(kmx),source=statN_t(order=order))
    allocate(rho(kmx),source=statN_t(order=order))
    allocate(  T(kmx),source=statN_t(order=order))
    allocate(  p(kmx),source=statN_t(order=order))
    allocate( T0(kmx),source=statN_t(order=order))

    ! Set the factory type to use to construct the covariance array objects
    allocate(covN_factory_t :: covar_factory)
    allocate(uT(kmx),uw(kmx),rhoT(kmx),rhou(kmx),rhow(kmx),up(kmx))

    do k = 1,kmx
       call covar_factory%Create(  uT(k)%cov  ,u(k),T(k))
       call covar_factory%Create(  uw(k)%cov  ,u(k),w(k))
       call covar_factory%Create(rhoT(k)%cov,rho(k),T(k))
       call covar_factory%Create(rhou(k)%cov,rho(k),u(k))
       call covar_factory%Create(rhow(k)%cov,rho(k),w(k))
       call covar_factory%Create(  up(k)%cov  ,u(k),p(k))
    end do
  end subroutine allocate_init_spocs

  subroutine allocate_init_non_spocs()
    allocate(umean(kmx),uvar(kmx),u3rd(kmx),u4th(kmx),vmean(kmx),vvar(kmx),wmean(kmx),wvar(kmx),&
         pmean(kmx),pvar(kmx),p3rd(kmx),p4th(kmx),Tmean(kmx),Tvar(kmx),T3rd(kmx),T4th(kmx),&
         rhomean(kmx),uTcov(kmx),uwcov(kmx),T0mean(kmx),T0var(kmx))
    umean   = 0; uvar  = 0; u3rd = 0; u4th = 0
    vmean   = 0; vvar  = 0
    wmean   = 0; wvar  = 0
    pmean   = 0; pvar  = 0; p3rd = 0; p4th = 0
    Tmean   = 0; Tvar  = 0; T3rd = 0; T4th = 0
    uTcov   = 0; uwcov = 0
    T0mean  = 0; T0var = 0
  end subroutine allocate_init_non_spocs

  subroutine write_convergence_tcplt_hdr(lun)
    integer ,intent(out) :: lun
    ! write the header data to a time series/convergence plot for some variables, overwriting past outputs
    open(newunit=lun,file=convergence_fl,position='rewind',form='formatted')
    write(lun,'(A)',advance='no') 'TITLE = "time convergence", VARIABLES = "Nsamples" "z" "time" '
    write(lun,'(A)',advance="no") '"umean" "uvar" "u3rd" "u4th" "u_kappa" "u_error_bound" '
    write(lun,'(A)',advance='no') '"Tmean" "Tvar" "T3rd" "T4th" "T_kappa" "T_error_bound" '
    write(lun,'(A)',advance='no') '"pmean" "pvar" "p3rd" "p4th" "p_kappa" "p_error_bound" '
    write(lun,'(A)')              '"T0mean" "T0var" "T03rd" "T04th" "T0_kappa" "T0_error_bound"'
    write(lun,'(A, I0, A, I0)') 'ZONE ZONETYPE = Ordered, DATAPACKING = point, I = ', kmx, ', J = ', nhdirs
  end subroutine write_convergence_tcplt_hdr

  subroutine write_spocs_tecplot_hdr(n)
    integer, intent(in) :: n
    integer :: lun
    ! Write out the statistics as they were just computed
    open(newunit=lun,file=pick_fl(spocs_stats_fl,n,WP),status='replace',form='formatted',position='rewind')
    write(lun,'(A)',advance="no") 'VARIABLES = "z" "nsamples" "umean" "uvar" "u3rd" "u4th" '
    write(lun,'(A)',advance="no") '"vmean" "vvar" "v3rd" "v4th" '
    write(lun,'(A)',advance="no") '"wmean" "wvar" "w3rd" "w4th" '
    write(lun,'(A)',advance="no") '"pmean" "pvar" "p3rd" "p4th" '
    write(lun,'(A)',advance="no") '"Tmean" "Tvar" "T3rd" "T4th" '
    write(lun,'(A)',advance="no") '"rhomean" "rhovar" "rho3rd" "rho4th" '
    write(lun,'(A)',advance="no") '"T0mean" "T0var" "T03rd" "T04th" '
    write(lun,'(A)',advance='no') '"R_ut" "cov(uT)" "R_uw" "cov(uw)" '
    write(lun,'(A)',advance='no') '"R_rhoT" "cov(rhoT)" "R_rhou" "cov(rhou)"'
    write(lun,'(A)')              '"R_rhow" "cov(rhow)" "R_up" "cov(up)"'
    write(lun,'(A,I0)') 'ZONE ZONETYPE=Ordered, DATAPACKING=point, I=', kmx
    close(lun)
  end subroutine write_spocs_tecplot_hdr

  subroutine write_txtbk_tecplot_hdr(n,lun)
    integer ,intent(in)  :: n
    integer ,intent(out) :: lun
    ! Write out the statistics as they were just computed
    open(newunit=lun,file=pick_fl(txtbk_stats_fl,n,WP),status='replace',form='formatted',position='rewind')
    write(lun,'(A)',advance="no") 'VARIABLES = "z" "nsamples" '
    write(lun,'(A)',advance="no") '"umean" "vmean" "wmean" "pmean" "Tmean" "rhomean" "T0mean" '
    write(lun,'(A)',advance="no") '"uvar" "u3rd" "u4th" '
    write(lun,'(A)',advance="no") '"vvar" '
    write(lun,'(A)',advance="no") '"wvar" '
    write(lun,'(A)',advance="no") '"pvar" "p3rd" "p4th" '
    write(lun,'(A)',advance="no") '"Tvar" "T3rd" "T4th" '
    write(lun,'(A)')              '"cov(uT)" "cov(uw)" "R_uT" "R_uw" "T0var"'
    write(lun,'(A,I0)') 'ZONE ZONETYPE=Ordered, DATAPACKING=block, I=', kmx
    ! end header info
  end subroutine

  subroutine write_txtbk_tecplot_fl(lun,kmx)
    integer ,intent(in) :: lun,kmx
    real(WP), dimension(kmx) :: ubar, vbar, wbar, pbar, Tbar, rhobar, &
         u2tmp, v2tmp, w2tmp, T2tmp, p2tmp, u3tmp, p3tmp, T3tmp, u4tmp, p4tmp, &
         T4tmp, uTtmp, uwtmp
    ! update state:
    ! Now compute the variances and covariances using the textbook formula
    NptsInv = 1.0/(curhdir*nspan*(iend - ibeg + 1))
    uTtmp = NptsInv*(uTcov - NptsInv*umean*Tmean)
    uwtmp = NptsInv*(uwcov - NptsInv*umean*wmean)
    ! means
    ubar   = NptsInv*umean
    vbar   = NptsInv*vmean
    wbar   = NptsInv*wmean
    pbar   = NptsInv*pmean
    Tbar   = NptsInv*Tmean
    rhobar = NptsInv*rhomean
    ! variances
    u2tmp = NptsInv*uvar - ubar*ubar
    v2tmp = NptsInv*vvar - vbar*vbar
    w2tmp = NptsInv*wvar - wbar*wbar
    p2tmp = NptsInv*pvar - pbar*pbar
    T2tmp = NptsInv*Tvar - Tbar*Tbar
    ! compute higher order moments
    u3tmp = NptsInv*u3rd - 3*ubar*u2tmp - ubar**3
    p3tmp = NptsInv*p3rd - 3*pbar*p2tmp - pbar**3
    T3tmp = NptsInv*T3rd - 3*Tbar*T2tmp - Tbar**3
    u4tmp = NptsInv*u4th - 4*ubar*u3tmp - 6*u2tmp*(ubar**2) - ubar**4
    p4tmp = NptsInv*p4th - 4*pbar*p3tmp - 6*p2tmp*(pbar**2) - pbar**4
    T4tmp = NptsInv*T4th - 4*Tbar*T3tmp - 6*T2tmp*(Tbar**2) - Tbar**4

    ! Now write out the data
    write(lun,rl_fmt) (z(k),k=1,kmx)
    write(lun,'(I0)') (curhdir*nspan*(iend - ibeg + 1),k=1,kmx)
    write(lun,rl_fmt) ubar
    write(lun,rl_fmt) vbar
    write(lun,rl_fmt) wbar
    write(lun,rl_fmt) pbar
    write(lun,rl_fmt) Tbar
    write(lun,rl_fmt) rhobar
    write(lun,rl_fmt) T0mean*NptsInv
    write(lun,rl_fmt) u2tmp
    write(lun,rl_fmt) u3tmp
    write(lun,rl_fmt) u4tmp
    write(lun,rl_fmt) v2tmp
    write(lun,rl_fmt) w2tmp
    write(lun,rl_fmt) p2tmp
    write(lun,rl_fmt) p3tmp
    write(lun,rl_fmt) p4tmp
    write(lun,rl_fmt) T2tmp
    write(lun,rl_fmt) T3tmp
    write(lun,rl_fmt) T4tmp
    write(lun,rl_fmt) uTtmp
    write(lun,rl_fmt) uwtmp
    do k = 1,kmx
       if ( u2tmp(k) > 0.0_WP .and. T2tmp(k) > 0.0_WP) then
          write(lun,rl_fmt) uTtmp(k)/(sqrt(u2tmp(k))*sqrt(T2tmp(k)))
       else
          write(lun,rl_fmt) sign(huge(uTtmp(k)),uTtmp(k))
       end if
    end do
    do k = 1,kmx
       if ( u2tmp(k) > 0.0_WP .and. w2tmp(k) > 0.0_WP) then
          write(lun,rl_fmt) uwtmp(k)/(sqrt(u2tmp(k)*sqrt(w2tmp(k))))
       else
          write(lun,rl_fmt) sign(huge(uwtmp(k)),uwtmp(k))
       end if
    end do
    write(lun,rl_fmt) NptsInv*(T0var - NptsInv*T0mean*T0mean)
    close(lun)
  end subroutine

  ! Get the nth command line argument which should be the name of the next H-directory to process
  subroutine get_arg_n(n)
    integer(WI) ,intent(in) :: n

    if (allocated(h_dir)) deallocate(h_dir)
    call get_command_argument(n,length=arglen,status=status)
    if (status > 0) stop "Problem retrieving command line argument"
    allocate(character(len=arglen)::h_dir)
    call get_command_argument(n,value=h_dir,status=status)
    if (status > 0) stop "Problem retrieving command line argument"
    if (status == -1) stop "String too short to contain command line argument"
  end subroutine get_arg_n

  elemental function tot_temp(T,u,v,w) result(res)
    real(DP) ,intent(in) :: T,u,v,w
    real(WP) :: res
    res = T + 0.5_WP*Cp_inv*(u**2 + v**2 + w**2)
  end function tot_temp

  function pick_fl(basename,n,rkind,next) result(res)
    ! Determine the name of the output file to write based on:
    !   1. The precision. Relative errors for statistics computed in single precision
    !      are found by comparing them to statistics computed with the updated SPOCS
    !      algorithm using double precision. One pass of the code is in single precision,
    !      the next is done in double precision to compute the "truth" and single precision
    !      for two pass algorithms
    !   2. To look at the effect of sample size on relative error output statistics for every
    !      2^n iterations. For updating formulas round up: e.g. output statistics every
    !      every snapshot visited, overwriting the file until 2^n snapshots are visited,
    !      computed and written to a file, then write to the next file until it is a power
    !      of two again. (e.g., when visiting snapshots 5-8 working on the single precision
    !      pass, output to `<file>_SP_8.dat`. When working with snapshots 9-16 output to
    !      "<file>_SP_16.dat" etc.)
    character(len=*) ,intent(in) :: basename
    integer(WI) ,intent(in) :: n,rkind
    logical ,optional ,intent(in) :: next
    character(len=:),allocatable :: res
    character(len=:),allocatable :: twos
    integer(WI) :: two_power, inext
    character(len=4) :: prec
    logical :: lnext

    lnext = .false.
    if ( present(next) ) lnext = next
    inext = 0_WI
    if (lnext) inext = 1_WI
    two_power = 2_WI**( ceiling( log2( real(n,DP) ) ) + inext)
    allocate(twos,source=int2str(two_power))
    if (rkind == DP) then
       prec = "_DP_"
    else if (rkind == SP) then
       prec = "_SP_"
    else
       stop "Error, invalid rkind/WP passed to pick_fl"
    end if
    allocate(res,source=trim(basename)//prec//twos//".dat")
  end function pick_fl

  elemental function log2(x) result(res)
    real(DP) ,intent(in) :: x
    real(DP) :: res
    res = log(x)*l2_inv
  end function log2

  elemental function ndigits(int) result(res)
    integer(WI), intent(in) :: int
    integer(WI) :: res
    res = ceiling( log10( abs(int) + 1.0_DP )) + merge(1,0,int<0)
  end function ndigits

  function int2str(n) result(res)
    integer(WI), intent(in) :: n
    character(len=:), allocatable :: res
    integer(WI) :: ndgts
    ndgts = ndigits(n)
    allocate(character(len=ndgts) :: res)
    write(res,'(I0)') n
  end function int2str

end program spocs_accuracy
