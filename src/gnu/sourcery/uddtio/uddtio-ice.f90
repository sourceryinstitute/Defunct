module object_interface
  implicit none

  type, abstract :: object
  contains
    procedure(write_formatted_interface), deferred :: write_formatted
    generic :: write(formatted) => write_formatted
  end type

  abstract interface
    subroutine write_formatted_interface(this, unit, iotype, vlist, iostat, iomsg)
      import object
      implicit none
      class(object), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: vlist(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
    end subroutine
  end interface

  type, abstract, extends(object) :: oracle
  contains
    procedure(subtract_interface), deferred :: subtract
    generic :: operator(-) => subtract
  end type

  abstract interface
    function subtract_interface(this, rhs) result(difference)
      import oracle
      implicit none
      class(oracle), intent(in) :: this, rhs
      class(oracle), allocatable :: difference
    end function
  end interface

  type, public, extends(oracle) :: results_t
    character(len=:), allocatable :: header(:)
    real, allocatable :: body(:,:)
  contains
    procedure :: write_formatted
    procedure :: subtract
  end type

  interface

    module subroutine write_formatted(this, unit, iotype, vlist, iostat, iomsg)
      implicit none
      class(results_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: vlist(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
    end subroutine

    module function subtract(this, rhs) result(difference)
      implicit none
      class(results_t), intent(in) :: this
      class(oracle), intent(in) :: rhs
      class(oracle), allocatable :: difference
    end function

  end interface

end module

  use object_interface, only : results_t
  type(results_t) actual, expected
  write(*,*) actual - expected
end program
