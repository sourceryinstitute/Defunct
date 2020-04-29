module foo_module
  interface
    module subroutine foo()
    end subroutine
  end interface
end module

submodule(foo_module) foo_submodule
contains
  module procedure foo
  contains
    subroutine bar()
    end subroutine
  end procedure
end submodule
