program main
  !! Demonstrate gfortran Bug 78640 
  !! - [F2015] gfortran accepts invalid allocatable polymorphic result in pure function
  type foo
  end type
  type(foo) :: bar
  bar = f()
contains
  pure function f() result(y)
    !! Constraint C1589 in Draft 16-007r2 of the Fortran 2015 standard states
    !! "A pure function shall not have a polymorphic allocatable result."
    !! A 20161127 build of gfortran on MacOS accepts this code
    class(foo), allocatable :: y
    allocate(foo::y)
  end function
end program
