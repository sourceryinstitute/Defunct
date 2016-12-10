module shot_interface
  use parabola_interface, only : parabola
  implicit none
  type ,extends(parabola) :: shot
  contains
    procedure :: from_the_field
  end type
  interface
    pure module subroutine from_the_field(this)
       implicit none
       class(shot) :: this
    end subroutine
   end interface
end module
