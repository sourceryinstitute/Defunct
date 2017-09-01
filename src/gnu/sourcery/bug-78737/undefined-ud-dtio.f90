module object_interface
  type, abstract :: object
  contains
    procedure(write_formatted_interface), deferred ::write_formatted 
    generic :: write(formatted)=>write_formatted
  end type 
  abstract interface
    subroutine write_formatted_interface(this,unit,iotype,vlist,iostat,iomsg)
      import object
      class(object), intent(in) :: this
      integer, intent(in) :: unit
      character (len=*), intent(in) :: iotype
      integer, intent(in) :: vlist(:)
      integer, intent(out) :: iostat
      character (len=*), intent(inout) :: iomsg
    end subroutine
  end interface
  type, extends(object) :: non_abstract_child 
  contains
    procedure :: write_formatted
  end type
contains
  subroutine write_formatted(this,unit,iotype,vlist,iostat,iomsg)
    class(non_abstract_child), intent(in) :: this
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
  end subroutine
  subroutine assert(a)
    class(object):: a
    write(*,*) a 
  end subroutine
end module
   
end
