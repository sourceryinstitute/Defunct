module foo
  implicit none
  
  type :: Wrapper_2d
     class(*), allocatable :: elements(:,:)
  end type Wrapper_2d

  interface Wrapper
     module procedure new_wrapper_2d
  end interface Wrapper

contains

  function new_wrapper_2d(array) result(w)
    type(Wrapper_2d) :: w
    class(*), intent(in) :: array(:,:)

    w%elements = array
  end function new_wrapper_2d

end module foo

program main
  use foo
  implicit none

  class(*), allocatable :: obj
  type(Wrapper_2d) :: w
  integer :: expected(3,4)

  expected = 1
  w = Wrapper(expected)
  obj = w

end program main
