! CMake issue report #16234.  See https://gitlab.kitware.com/cmake/cmake/issues/16234
module foo_interface
  implicit none
  interface 
    module subroutine foo() 
    end subroutine 
  end interface 
end module 

submodule (foo_interface) foo_implementation
  implicit none
contains
  module subroutine foo() 
  end subroutine 
end submodule 
 
end  ! Dummy main program to produce an executable file
