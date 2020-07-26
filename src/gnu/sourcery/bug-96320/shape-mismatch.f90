module foobar
  type foo
  contains
    procedure, nopass :: bar
  end type
  interface
    module subroutine bar(arg)
      character(len=*) arg(:)
    end subroutine
  end interface
contains
  module procedure bar
  end procedure
end module
