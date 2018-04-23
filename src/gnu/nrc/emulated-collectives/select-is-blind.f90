module emulated_collectives_interface
  implicit none
  interface co_broadcast
    module procedure co_broadcast_integer
  end interface
  interface
    module subroutine co_broadcast_integer(a,source_image,stat,errmsg)
      implicit none
      integer, intent(inout) :: a
      integer, intent(in) :: source_image
      integer, intent(out), optional ::  stat
      character(len=*), intent(inout), optional :: errmsg
    end subroutine
  end interface
end module emulated_collectives_interface

submodule(emulated_collectives_interface) emulated_collectives_implementation
  implicit none
contains
  module procedure co_broadcast_integer
    integer, save :: message[*]
    integer, parameter :: root=1
    integer   me, parent, even_child, odd_child, image, my_node
    integer, allocatable :: node(:)
    me=this_image()
    print *,source_image
     select case(me)
       case(source_image)
         my_node = root
     end select
  end procedure
end submodule emulated_collectives_implementation

