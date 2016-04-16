module foo_module
  implicit none
  type foo
  contains
    final :: hello
  end type
contains
  subroutine hello(this)
    type(foo) :: this
    print *,"Hello from finalizer."
  end subroutine
end module
program main
  use foo_module
  implicit none
  block
    type(foo) :: bar
  end block
end program
