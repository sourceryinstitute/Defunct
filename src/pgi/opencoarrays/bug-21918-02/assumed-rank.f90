! pgfortran 15.7-0 64-bit target on Apple OS/X -tp sandybridge 
! lacks support for Fortran 2015 assumed-rank dummy arguments
! and the "rank" intrinsic function.
!
! The original lines from the OpenCoarrays source file
! src/extensions/opencoarrays.F90 are
!
! function gfc_descriptor_c_int(a) result(a_descriptor)
!   integer(c_int), intent(in), target, contiguous :: a(..)
! ...
!   a_descriptor%dtype = my_dtype(type_=BT_INTEGER,kind_=int(c_sizeof(a)/bytes_per_word,c_int32_t),rank_=rank(a))
!
! A workaround is to write seperate implementations for each desired rank. :(
!
  implicit none
contains
  subroutine foo(bar) 
    integer bar(..)
    print *,rank(bar)
  end subroutine
end
