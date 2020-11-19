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
      integer, intent(in) :: unit, vlist(:)
      character(len=*), intent(in) :: iotype
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
    end subroutine
  end interface

  type, abstract, extends(object) :: oracle
  contains
    procedure(negative_interface), deferred :: negative
    generic :: operator(-) => negative
  end type

  abstract interface
    function negative_interface(this)
      import oracle
      implicit none
      class(oracle), intent(in) :: this
      class(oracle), allocatable :: negative_interface
    end function
  end interface

  type, extends(oracle) :: results_t
    integer :: i=1
  contains
    procedure write_formatted
    procedure negative
  end type

contains

    subroutine write_formatted(this, unit, iotype, vlist, iostat, iomsg)
      implicit none
      class(results_t), intent(in) :: this
      integer, intent(in) :: unit, vlist(:)
      character(len=*), intent(in) :: iotype
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
      print *, this%i
      iostat = 0
    end subroutine

    function negative(this)
      implicit none
      class(results_t), intent(in) :: this
      class(oracle), allocatable :: negative
      type(results_t) negative_this
      negative_this%i = - this%i
      negative = negative_this
    end function

end module

  use object_interface
  write(*,*) -results_t()
end
