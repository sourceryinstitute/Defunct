module wrapped_classes
  implicit none
  private
  public :: wrapped_coarray

  type :: wrapped_point
    integer, allocatable :: point(:)
    contains
      procedure :: add => wrapped_point_add
  end type wrapped_point

  type :: wrapped_coarray
    type(wrapped_point), allocatable :: caf(:)[:]
  end type wrapped_coarray

  contains

    subroutine wrapped_point_add(self, to_add)
      class(wrapped_point), intent(inout) :: self
      integer,              intent(in)    :: to_add

      if (allocated(self%point)) then
        self%point = [self%point, to_add]
      else
        self%point = [to_add]
      end if
    end subroutine wrapped_point_add
end module wrapped_classes

program test
  use wrapped_classes
  implicit none

  type(wrapped_coarray) :: foo
  allocate(foo%caf(99)[*])
  call foo%caf(32)%add(this_image())
  print*, foo%caf(32)%point
end program test

