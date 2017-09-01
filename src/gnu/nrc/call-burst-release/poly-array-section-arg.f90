 !! Gfortran 7.1.0 and 8.0.0 20170731 report an ICE when a
 !! polymorphic 1D array dummy argument of a child type is
 !! associated with a type-guarded, 1D section of a 2D
 !! polymorphic array actual argument declared as the parent
 !! type. Gfortran 5.4.0 and 6.4.0 compile the code without
 !! reporting any errors.
    type parent
    end type parent
    type, extends(parent) :: child
    end type
    class(parent), allocatable :: foo(:,:)
    allocate(child::foo(1,1))
    select type(foo)
      class is (child)
        call gfortran7_ICE(foo(1,:))
    end select
contains
    subroutine gfortran7_ICE(bar)
      class(child) bar(:)
    end subroutine
end
