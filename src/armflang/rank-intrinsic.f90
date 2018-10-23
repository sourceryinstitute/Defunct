program main
  implicit none
  integer :: a(1,1)
  if (rank(a)/=2) error stop
end program
