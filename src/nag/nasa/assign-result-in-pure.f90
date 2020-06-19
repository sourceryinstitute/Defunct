module bar_interface
  interface
    pure module function bar() result(one)
      integer one
    end function
  end interface
contains
  module procedure bar
    one = 1
  end procedure
end module
