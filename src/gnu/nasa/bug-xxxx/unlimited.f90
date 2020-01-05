  implicit none

  type Wrapper
    class(*), allocatable :: elements(:)
  end type

  class(*), allocatable :: obj
  type(Wrapper) w
  integer :: expected(1)=1

  w = new_wrapper(expected)
  ! The following assignment causes a segmentation fault at runtime:
  obj = w
  ! The following assignment generates an "undefined symbols" linker
  ! error but only if the above "obj = w" assignment is removed:
  obj =  new_wrapper(expected)
contains
  type(Wrapper) function new_wrapper(array)
    class(*) array(:)
    new_wrapper%elements = array
  end function
end
