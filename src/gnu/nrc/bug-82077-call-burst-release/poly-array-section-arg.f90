 !! Gfortran 7.1.0 and 8.0.0 20170731 report an ICE when a
 !! polymorphic array dummy argument is associated with a
 !! a type-guarded, section of a polymorphic array actual
 !! argument. Gfortran 5.4.0 and 6.4.0 compile the code
 !! without reporting any errors.
  type :: child
  end type
  class(child), allocatable :: foo(:)
  allocate(foo(1))
  select type(foo)
    class is (child)
      call gfortran7_ICE(foo(1:1))
  end select
contains
  subroutine gfortran7_ICE(bar)
    class(child) bar(:)
  end subroutine
end
