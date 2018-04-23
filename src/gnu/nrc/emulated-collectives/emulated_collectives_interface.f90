module emulated_collectives_interface
  !! Fortran 2008 coarray emulations of Fortran 2018 intrinsic collective subroutines
  implicit none

contains

    module subroutine co_sum_integer(a,result_image,stat,errmsg)
      !! parallel computation of the sum of the first argument
      implicit none
      integer, intent(inout) :: a
      integer, intent(in), optional :: result_image
      integer, intent(out), optional ::  stat
      character(len=*), intent(inout), optional :: errmsg

     !! Binary tree collective sum reduction

     integer, save :: total[*]
     integer, parameter :: root=1

     total = a

     associate( me=>this_image() )

       associate(even_child=>2*me)
         if ( exists(even_child) ) then
           sync images(even_child)
           total = total + total[even_child]
         end if
       end associate

       associate(odd_child=>2*me+1)
         if ( exists(odd_child) ) then
           sync images(odd_child)
           total = total + total[odd_child]
         end if
       end associate

       associate(parent=>me/2)
         if (exists(parent)) sync images(parent)
           !! start accumulation up the tree when the first childless image falls through this condition
       end associate

       if (me==root) a = total
       if (present(result_image)) then
         if (result_image/=root) then
           if (me==root) sync images(result_image)
           if (me==result_image) then
             sync images(root)
             a = total[root]
           end if
         end if
       else
         call co_broadcast_integer(total,source_image=root)
         a=total
       end if

     end associate

     if (present(errmsg)) errmsg=""
     if (present(stat)) stat=0

    end subroutine

    module subroutine co_broadcast_integer(a,source_image,stat,errmsg)
      !! parallel one-to-all communication of the value of first argument
      implicit none
      integer, intent(inout) :: a
      integer, intent(in) :: source_image
      integer, intent(out), optional ::  stat
      character(len=*), intent(inout), optional :: errmsg

     !! Binary tree collective broadcast

     integer, save :: message[*]
     integer, parameter :: root=1
     integer, allocatable :: node(:)

     associate(me=>this_image(), ni=>num_images())

       block
         integer image
         node = [(image,image=1,ni)]
       end block

       node(source_image) = root  !! swap ordering of root & source images
       node(root) = source_image

       associate(parent=>me/2)
         if (exists(parent)) then
           print *,me,"syncs with parent",node(parent)
           sync images(node(parent))
           message = message[node(parent)]
           print *,me,"gets message",message,"from parent",node(parent)
         end if
       end associate

       associate(even_child=>2*me)
         print *,me,"syncs with even child",node(even_child)
         if ( exists(even_child) ) sync images(node(even_child))
       end associate

       associate(odd_child=>2*me+1)
         print *,me,"syncs with odd child",node(odd_child)
         if ( exists(odd_child) ) sync images(node(odd_child))
       end associate

     end associate

     stop "stopped inside co_broadcast_integer"

     a = message

     if (present(errmsg)) errmsg=""
     if (present(stat)) stat=0

  end subroutine

  pure function exists(image) result(image_exists)
     !! Result true if image number is within the closed range [1,num_images()]
     integer, intent(in) :: image
     logical image_exists
     image_exists = (image>0 .and. image<=num_images())
  end function

end module
