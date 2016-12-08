  !! gfortran 6.2.0 and 7.0.0 20161127 report an ICE
  !! when a polymorphic allocatable function result
  !! appears in an array constructor
  type foo
  end type
  type(foo) :: bar(1)
  bar = [f()]
contains
  function f() result(foobar)
     class(foo), allocatable :: foobar
    allocate(foobar,source = foo())
  end function
end program
