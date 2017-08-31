  use iso_fortran_env
  implicit none
  print *, integer_kinds
  call testsub
contains
  subroutine testsub
    use iso_fortran_env
    print * , integer_kinds
  end subroutine
end
