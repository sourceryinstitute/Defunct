  implicit none

  type foo
     logical, allocatable :: bar[:]
  end type

contains
  subroutine foobar(this)
    class(foo), intent(out) :: this
  end subroutine
end
