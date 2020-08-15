module foobar
  implicit none

  type foo
    integer bar
  end type

  interface
    pure module function create() result(new_foo)
      implicit none
      type(foo) new_foo
    end function
  end interface

contains
  module procedure create
    new_foo%bar = 1
  end procedure
end module
