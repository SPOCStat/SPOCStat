  ! If fortran code generated with the intel compiler, but called from
  ! c or c++ there is a chance that the Fortran run-time library will
  ! not be properly initialized. This can result in segmentation
  ! faults and weird behavior, for example, when run time checking
  ! with the -check flag is enabled. Apparently, performing IO will
  ! force initialization of the Fortran run-time library (FRTL). Here
  ! we try to write a message to the great bit bucket in the sky if
  ! we can find it, otherwise to std-out or it's windows equivalent.

#ifdef __INTEL_COMPILER
#  ifdef _WIN32
#    define NULL "NUL"
#  endif
#  ifdef __unix__
#    define NULL "/dev/null"
#  endif
#  ifdef __APLE__
#    define NULL "/dev/null"
#  endif
#  ifdef __MACH__
#    define NULL "/dev/null"
#  endif
#  ifndef NULL
     write(*,*) 'Attempting to initialize the Intel Fortran run-time l&
          &ibrary.'
#   else
     open(unit=111,file=NULL,status='old',action='write') ! not super
     ! safe, but include files are a bit awkward in Fortran because
     ! executable statements can't come before declarations and
     ! vice-versa so
     write(111,*) 'Attempting to initialized the Intel Fortran run-tim&
          &e library.'
     close(111)
#  endif
#  ifdef NULL
#    undef NULL
#  endif
#endif
