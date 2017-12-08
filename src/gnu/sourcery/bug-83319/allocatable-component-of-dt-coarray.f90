module foo_module
  implicit none
  type foo
    integer, allocatable :: i(:)
  end type
end module

  use foo_module
  implicit none
  type(foo), save :: bar[*]
  allocate(bar%i(1))
end
