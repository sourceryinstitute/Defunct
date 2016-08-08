module my_interface
  implicit none
  interface
    pure module subroutine f
    end subroutine
  end interface
end module 

submodule(my_interface) my_implementation
  implicit none
contains
    pure module subroutine f
    end subroutine
end submodule

print *,"Test passed."
end
