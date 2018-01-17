module foo_module
  implicit none
  type foo
    integer, allocatable :: bar(:)
  contains
    procedure :: ones
  end type
contains
  function ones(this)
    class(foo) this
    integer ones(size(this%bar))
    ones=1
  end function
end module

contains
  subroutine print_ones(object)
    use foo_module
    implicit none
    class(foo) object
    print *, object%ones()
  end subroutine
end
