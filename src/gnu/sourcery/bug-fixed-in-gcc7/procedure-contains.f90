module foo_interface
  implicit none
  interface
    module subroutine foo()
    end subroutine
  end interface
end module foo_interface

submodule(foo_interface) foo_implementation
contains
    module procedure foo
    contains
      module subroutine bar()
      end subroutine
    end procedure
   !end subroutine ! gfortran accepts this invalid workaround
end submodule 
