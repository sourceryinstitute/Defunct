! pgfortran complains about giving the SAVE and BIND(C) attributes to a variable:
integer, save, bind(C) :: CAF_COMM_WORLD
end 
!
! The original line from the module named "opencoarrays" in the OpenCoarrays
! source file src/extensions/opencoarrays.F90 is
!
! integer(c_int), save, volatile, bind(C,name="CAF_COMM_WORLD") :: CAF_COMM_WORLD
!
! A workaround is to remove the "save" attribute.  Fortran 2003 specifies that module 
! variables the SAVE attribute by default so this workaround should have no impact
! when using a standards-compliant compiler.
