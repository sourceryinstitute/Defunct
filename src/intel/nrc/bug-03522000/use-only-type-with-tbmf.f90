module foo_module
  type foo
  contains
    procedure, nopass :: bar
  end type
  interface 
    module function bar() result(x)
      real x
    end function
  end interface
end module

module foobar_module 
  interface
    subroutine foobar()
      use foo_module, only : foo
    end subroutine
  end interface
end module
