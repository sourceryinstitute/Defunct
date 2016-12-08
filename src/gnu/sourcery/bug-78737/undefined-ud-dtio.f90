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
contains
  subroutine assert(a)
    class(object):: a
    write(*,*) a 
  end subroutine
end module
   
end
