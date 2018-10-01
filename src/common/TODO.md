[ ] Contemplate refactoring the stringhelper module/class to just
    overload assignment and concatenation operators operating on
    deferred length allocatable scalar characters. This *should* make
    the code compatible with gfortran 4.8. (See the commit notes for
    r545.) This might also be a potential work around for Intel's
    ifort bug exercised by the overloaded concatenation
    operator(//). This bug has been submitted to Intel via my premier
    support account (I BETA test for them) but I have yet to hear back
    that they have escalated the issue, or made any progress on it.
