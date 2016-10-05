MODULE abstract_parent
  implicit none

  type, abstract :: parent
  contains
    procedure(write_formatted_interface), deferred :: write_formatted
    generic :: write(formatted) => write_formatted
  end type parent

  abstract interface
    subroutine write_formatted_interface(this,unit,iotype,vlist,iostat,iomsg)
      import parent
      class(parent), intent(in) :: this
      integer, intent(in) :: unit
      character (len=*), intent(in) :: iotype
      integer, intent(in) :: vlist(:)
      integer, intent(out) :: iostat
      character (len=*), intent(inout) :: iomsg
    end subroutine
  end interface

end module

module child_module
  use abstract_parent, only : parent
  implicit none

  type, extends(parent) :: child
  contains
    procedure :: write_formatted
  end type
contains
  subroutine write_formatted(this,unit,iotype,vlist,iostat,iomsg)
    class(child), intent(in) :: this
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
  end subroutine
end module

  use child_module, only : child
  implicit none
  write(*,*) child()
end 
