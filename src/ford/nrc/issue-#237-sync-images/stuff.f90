module stuff
  interface
    module subroutine foo()
    end subroutine
  end interface
end module

submodule(stuff) sub_stuff
  implicit none
contains
  module procedure foo
    sync images(1)
  end procedure
end submodule
