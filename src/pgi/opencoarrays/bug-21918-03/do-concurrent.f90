! pgfortran 15.7-0 64-bit target on Apple OS/X -tp sandybridge 
! lacks support for the Fortran 2008 "do concurrent" statement
!
! The first offending line in the OpenCoarrays source file 
! src/extensions/opencoarrays.F90 is
!   do concurrent(i=1:rank(a))
!
! Below is a minimal reproducer
do concurrent(i=1:1)
end do
end
