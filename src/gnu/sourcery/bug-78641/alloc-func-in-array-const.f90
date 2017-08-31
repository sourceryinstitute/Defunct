  !! gfortran 6.4.0, 7.1.0, and 8.0.0 20170731 report an
  !! ICE when a polymorphic allocatable function result
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
